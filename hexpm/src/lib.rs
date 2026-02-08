mod proto;

#[cfg(test)]
mod tests;

pub mod version;

use crate::proto::{signed::Signed, versions::Versions};
use bytes::buf::Buf;
use ecow::EcoString;
use flate2::read::GzDecoder;
use http::{Method, StatusCode};
use prost::Message;
use regex::Regex;
use ring::digest::{Context, SHA256};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::sync::OnceLock;
use std::time::{Duration, Instant};
use std::{
    collections::HashMap,
    convert::{TryFrom, TryInto},
    fmt::Display,
    io::{BufReader, Read},
};
use thiserror::Error;
use version::{Range, Version};
use x509_parser::prelude::FromDer;

#[derive(Debug, Clone)]
pub struct Config {
    /// Defaults to https://hex.pm/api/
    pub api_base: http::Uri,
    /// Defaults to https://repo.hex.pm/
    pub repository_base: http::Uri,
}

impl Config {
    pub fn new() -> Self {
        Self {
            api_base: http::Uri::from_static("https://hex.pm/api/"),
            repository_base: http::Uri::from_static("https://repo.hex.pm/"),
        }
    }

    fn api_request(&self, method: http::Method, path_suffix: &str) -> RequestBuilder {
        RequestBuilder {
            builder: make_request(self.api_base.clone(), method, path_suffix)
                .header("content-type", "application/json")
                .header("accept", "application/json"),
        }
    }

    fn repository_request(
        &self,
        method: http::Method,
        path_suffix: &str,
        credentials: Option<&Credentials>,
    ) -> http::request::Builder {
        RequestBuilder {
            builder: make_request(self.repository_base.clone(), method, path_suffix),
        }
        .read_credentials(credentials)
    }
}

impl Default for Config {
    fn default() -> Self {
        Self::new()
    }
}

struct RequestBuilder {
    builder: http::request::Builder,
}

impl RequestBuilder {
    pub fn read_credentials(self, credentials: Option<&Credentials>) -> http::request::Builder {
        let mut builder = self.builder;
        match credentials {
            Some(Credentials::OAuthAccessToken(token)) => {
                builder = builder.header("authorization", format!("Bearer {token}"));
            }
            Some(Credentials::ApiKey(key)) => {
                builder = builder.header("authorization", key.to_string());
            }
            None => (),
        }
        builder
    }

    pub fn write_credentials(self, credentials: &WriteActionCredentials) -> http::request::Builder {
        let mut builder = self.builder;
        match credentials {
            WriteActionCredentials::OAuthAccessToken {
                access_token,
                one_time_password,
            } => {
                builder = builder.header("authorization", format!("Bearer {access_token}"));
                builder = builder.header("x-hex-otp", one_time_password.to_string());
            }
            WriteActionCredentials::ApiKey(key) => {
                builder = builder.header("authorization", key.to_string());
            }
        }
        builder
    }
}

#[derive(Debug, Clone)]
pub enum Credentials {
    // Short lived credential from OAuth
    OAuthAccessToken(EcoString),
    // Long lived API key
    ApiKey(EcoString),
}

#[derive(Debug, Clone)]
pub enum WriteActionCredentials {
    // Short lived credential from OAuth.
    // A one-time-password is required for write actions when using OAuth
    OAuthAccessToken {
        access_token: EcoString,
        one_time_password: EcoString,
    },
    // Long lived API key
    ApiKey(EcoString),
}

fn make_request(
    base: http::Uri,
    method: http::Method,
    path_suffix: &str,
) -> http::request::Builder {
    let mut parts = base.into_parts();
    parts.path_and_query = Some(
        match parts.path_and_query {
            Some(path_and_query) => {
                let mut path = path_and_query.path().to_owned();
                if !path.ends_with('/') {
                    path.push('/');
                }
                path += path_suffix;

                // Drop query parameters
                path.try_into()
            }
            None => path_suffix.try_into(),
        }
        .expect("api_uri path"),
    );
    let uri = http::Uri::from_parts(parts).expect("api_uri building");
    http::Request::builder()
        .method(method)
        .uri(uri)
        .header("user-agent", USER_AGENT)
}

/// Create a request that deletes an Hex API key.
///
/// API Docs:
///
/// https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.user.ex#L291
///
/// https://github.com/hexpm/hex/blob/main/lib/hex/api/key.ex#L15
pub fn api_remove_api_key_request(
    name_of_key_to_delete: &str,
    credentials: &WriteActionCredentials,
    config: &Config,
) -> http::Request<Vec<u8>> {
    let path = format!("keys/{name_of_key_to_delete}");
    config
        .api_request(Method::DELETE, &path)
        .write_credentials(credentials)
        .body(vec![])
        .expect("remove_api_key_request request")
}

/// Parses a request that deleted a Hex API key.
pub fn api_remove_api_key_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();

    match parts.status {
        StatusCode::NO_CONTENT | StatusCode::OK => Ok(()),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(unauthorised_response(&parts.headers)),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

fn unauthorised_response(headers: &http::HeaderMap) -> ApiError {
    let authenticate_header = headers
        .get("www-authenticate")
        .and_then(|header| header.to_str().ok())
        .unwrap_or_default();

    if authenticate_header.starts_with("Bearer realm=\"hex\", error=\"invalid_totp\"") {
        return ApiError::IncorrectOneTimePassword;
    }

    ApiError::InvalidCredentials
}

/// Retire an existing package release from Hex.
///
/// API Docs:
///
/// https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.retire.ex#L75
///
/// https://github.com/hexpm/hex/blob/main/lib/hex/api/release.ex#L28
pub fn api_retire_release_request(
    package: &str,
    version: &str,
    reason: RetirementReason,
    message: Option<&str>,
    credentials: &WriteActionCredentials,
    config: &Config,
) -> http::Request<Vec<u8>> {
    let body = json!({
        "reason": reason.to_str(),
        "message": message,
    });
    let path = format!("packages/{package}/releases/{version}/retire");
    config
        .api_request(Method::POST, &path)
        .write_credentials(credentials)
        .body(body.to_string().into_bytes())
        .expect("retire_release_request request")
}

/// Parses a request that retired a release.
pub fn api_retire_release_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::NO_CONTENT | StatusCode::OK => Ok(()),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(unauthorised_response(&parts.headers)),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

/// Un-retire an existing retired package release from Hex.
///
/// API Docs:
///
/// https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.retire.ex#L89
///
/// https://github.com/hexpm/hex/blob/main/lib/hex/api/release.ex#L35
pub fn api_unretire_release_request(
    package: &str,
    version: &str,
    credentials: &WriteActionCredentials,
    config: &Config,
) -> http::Request<Vec<u8>> {
    let path = format!("packages/{package}/releases/{version}/retire");
    config
        .api_request(Method::DELETE, &path)
        .write_credentials(credentials)
        .body(vec![])
        .expect("unretire_release_request request")
}

/// Parses a request that un-retired a package version.
pub fn api_unretire_release_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::NO_CONTENT | StatusCode::OK => Ok(()),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(unauthorised_response(&parts.headers)),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

/// Create a request that get the names and versions of all of the packages on
/// the package registry.
///
/// https://github.com/hexpm/specifications/blob/main/registry-v2.md
///
/// TODO: Where are the API docs for this?
pub fn repository_v2_get_versions_request(
    credentials: Option<&Credentials>,
    config: &Config,
) -> http::Request<Vec<u8>> {
    config
        .repository_request(Method::GET, "versions", credentials)
        .header("accept", "application/json")
        .body(vec![])
        .expect("get_repository_versions_request request")
}

/// Parse a request that gets the names and versions of all of the packages on
/// the package registry.
pub fn repository_v2_get_versions_response(
    response: http::Response<Vec<u8>>,
    public_key: &[u8],
) -> Result<HashMap<String, Vec<Version>>, ApiError> {
    let (parts, body) = response.into_parts();

    match parts.status {
        StatusCode::OK => (),
        status => return Err(ApiError::unexpected_response(status, body)),
    };

    let mut decoder = GzDecoder::new(body.reader());
    let mut body = Vec::new();
    decoder.read_to_end(&mut body)?;

    repository_v2_get_versions_body(&body, public_key)
}

/// Parse a signed binary message containing all of the packages on the package registry.
pub fn repository_v2_get_versions_body(
    protobuf_bytes: &Vec<u8>,
    public_key: &[u8],
) -> Result<HashMap<String, Vec<Version>>, ApiError> {
    let signed = Signed::decode(protobuf_bytes.as_slice())?;

    let payload =
        verify_payload(signed, public_key).map_err(|_| ApiError::IncorrectPayloadSignature)?;

    let versions = Versions::decode(payload.as_slice())?
        .packages
        .into_iter()
        .map(|n| {
            let parse_version = |v: &str| {
                let err = |_| ApiError::InvalidVersionFormat(v.to_string());
                Version::parse(v).map_err(err)
            };
            let versions = n
                .versions
                .iter()
                .map(|v| parse_version(v.as_str()))
                .collect::<Result<Vec<Version>, ApiError>>()?;
            Ok((n.name, versions))
        })
        .collect::<Result<HashMap<_, _>, ApiError>>()?;

    Ok(versions)
}

/// Create a request to get the information for a package in the repository.
///
/// https://github.com/hexpm/specifications/blob/main/registry-v2.md
///
pub fn repository_v2_get_package_request(
    name: &str,
    credentials: Option<&Credentials>,
    config: &Config,
) -> http::Request<Vec<u8>> {
    config
        .repository_request(Method::GET, &format!("packages/{name}"), credentials)
        .header("accept", "application/json")
        .body(vec![])
        .expect("get_package_request request")
}

/// Parse a response to get the information for a package in the repository.
///
pub fn repository_v2_get_package_response(
    response: http::Response<Vec<u8>>,
    public_key: &[u8],
) -> Result<Package, ApiError> {
    let (parts, body) = response.into_parts();

    match parts.status {
        StatusCode::OK => (),
        StatusCode::FORBIDDEN => return Err(ApiError::NotFound),
        StatusCode::NOT_FOUND => return Err(ApiError::NotFound),
        status => {
            return Err(ApiError::unexpected_response(status, body));
        }
    };

    let mut decoder = GzDecoder::new(body.reader());
    let mut body = Vec::new();
    decoder.read_to_end(&mut body)?;

    repository_v2_package_parse_body(&body, public_key)
}

/// Parse a signed binary message containing the information for a package in the repository.
pub fn repository_v2_package_parse_body(
    protobuf_bytes: &Vec<u8>,
    public_key: &[u8],
) -> Result<Package, ApiError> {
    let signed = Signed::decode(protobuf_bytes.as_slice())?;

    let payload =
        verify_payload(signed, public_key).map_err(|_| ApiError::IncorrectPayloadSignature)?;

    let package = proto::package::Package::decode(payload.as_slice())?;
    let releases = package
        .releases
        .clone()
        .into_iter()
        .map(proto_to_release)
        .collect::<Result<Vec<_>, _>>()?;
    let package = Package {
        name: package.name,
        repository: package.repository,
        releases,
    };

    Ok(package)
}

/// Create a request to download a version of a package as a tarball
/// TODO: Where are the API docs for this?
pub fn repository_get_package_tarball_request(
    name: &str,
    version: &str,
    credentials: Option<&Credentials>,
    config: &Config,
) -> http::Request<Vec<u8>> {
    config
        .repository_request(
            Method::GET,
            &format!("tarballs/{name}-{version}.tar"),
            credentials,
        )
        .header("accept", "application/x-tar")
        .body(vec![])
        .expect("get_package_tarball_request request")
}

/// Parse a response to download a version of a package as a tarball
///
pub fn repository_get_package_tarball_response(
    response: http::Response<Vec<u8>>,
    checksum: &[u8],
) -> Result<Vec<u8>, ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::OK => (),
        StatusCode::FORBIDDEN => return Err(ApiError::NotFound),
        StatusCode::NOT_FOUND => return Err(ApiError::NotFound),
        status => {
            return Err(ApiError::unexpected_response(status, body));
        }
    };
    let body = read_and_check_body(body.reader(), checksum)?;
    Ok(body)
}

/// API Docs:
///
/// https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.publish.ex#L384
///
/// https://github.com/hexpm/hex/blob/main/lib/hex/api/release_docs.ex#L19
pub fn api_remove_docs_request(
    package_name: &str,
    version: &str,
    credentials: &WriteActionCredentials,
    config: &Config,
) -> Result<http::Request<Vec<u8>>, ApiError> {
    validate_package_and_version(package_name, version)?;
    let path = format!("packages/{package_name}/releases/{version}/docs");
    Ok(config
        .api_request(Method::DELETE, &path)
        .write_credentials(credentials)
        .body(vec![])
        .expect("remove_docs_request request"))
}

pub fn api_remove_docs_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::NO_CONTENT => Ok(()),
        StatusCode::NOT_FOUND => Err(ApiError::NotFound),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(unauthorised_response(&parts.headers)),
        StatusCode::FORBIDDEN => Err(ApiError::Forbidden),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

/// API Docs:
///
/// https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.publish.ex#L429
///
/// https://github.com/hexpm/hex/blob/main/lib/hex/api/release_docs.ex#L11
pub fn api_publish_docs_request(
    package_name: &str,
    version: &str,
    gzipped_tarball: Vec<u8>,
    credentials: &WriteActionCredentials,
    config: &Config,
) -> Result<http::Request<Vec<u8>>, ApiError> {
    validate_package_and_version(package_name, version)?;
    let path = format!("packages/{package_name}/releases/{version}/docs");
    let mut builder = config
        .api_request(Method::POST, &path)
        .write_credentials(credentials);
    let headers = builder.headers_mut().expect("headers");
    headers.insert("content-encoding", "x-gzip".parse().unwrap());
    headers.insert("content-type", "application/x-tar".parse().unwrap());
    Ok(builder
        .body(gzipped_tarball)
        .expect("publish_docs_request request"))
}

pub fn api_publish_docs_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::CREATED => Ok(()),
        StatusCode::NOT_FOUND => Err(ApiError::NotFound),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(unauthorised_response(&parts.headers)),
        StatusCode::FORBIDDEN => Err(ApiError::Forbidden),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

/// API Docs:
///
/// https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.publish.ex#L512
///
/// https://github.com/hexpm/hex/blob/main/lib/hex/api/release.ex#L13
pub fn api_publish_package_request(
    release_tarball: Vec<u8>,
    credentials: &WriteActionCredentials,
    config: &Config,
    replace: bool,
) -> http::Request<Vec<u8>> {
    // TODO: do all the package tarball construction
    let path = format!("publish?replace={replace}");
    let mut builder = config
        .api_request(Method::POST, &path)
        .write_credentials(credentials);
    builder
        .headers_mut()
        .expect("headers")
        .insert("content-type", "application/x-tar".parse().unwrap());
    builder
        .body(release_tarball)
        .expect("publish_package_request request")
}

pub fn api_publish_package_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::OK | StatusCode::CREATED => Ok(()),
        StatusCode::NOT_FOUND => Err(ApiError::NotFound),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(unauthorised_response(&parts.headers)),
        StatusCode::FORBIDDEN => Err(ApiError::Forbidden),
        StatusCode::UNPROCESSABLE_ENTITY => {
            let body = &String::from_utf8_lossy(&body).to_string();
            if body.contains("--replace") {
                return Err(ApiError::NotReplacing);
            }
            Err(ApiError::LateModification)
        }
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

/// API Docs:
///
/// https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.publish.ex#L371
///
/// https://github.com/hexpm/hex/blob/main/lib/hex/api/release.ex#L21
pub fn api_revert_release_request(
    package_name: &str,
    version: &str,
    credentials: &WriteActionCredentials,
    config: &Config,
) -> Result<http::Request<Vec<u8>>, ApiError> {
    validate_package_and_version(package_name, version)?;
    let path = format!("packages/{package_name}/releases/{version}");
    Ok(config
        .api_request(Method::DELETE, &path)
        .write_credentials(credentials)
        .body(vec![])
        .expect("publish_package_request request"))
}

pub fn api_revert_release_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::NO_CONTENT => Ok(()),
        StatusCode::NOT_FOUND => Err(ApiError::NotFound),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(unauthorised_response(&parts.headers)),
        StatusCode::FORBIDDEN => Err(ApiError::Forbidden),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

/// See: https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.owner.ex#L47
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OwnerLevel {
    /// Has every package permission EXCEPT the ability to change who owns the package
    Maintainer,
    /// Has every package permission including the ability to change who owns the package
    Full,
}

impl Display for OwnerLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OwnerLevel::Maintainer => write!(f, "maintainer"),
            OwnerLevel::Full => write!(f, "full"),
        }
    }
}

/// API Docs:
///
/// https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.owner.ex#L107
///
/// https://github.com/hexpm/hex/blob/main/lib/hex/api/package.ex#L19
pub fn api_add_owner_request(
    package_name: &str,
    owner: &str,
    level: OwnerLevel,
    credentials: &WriteActionCredentials,
    config: &Config,
) -> http::Request<Vec<u8>> {
    let body = json!({
        "level": level.to_string(),
        "transfer": false,
    });
    let path = format!("packages/{package_name}/owners/{owner}");
    config
        .api_request(Method::PUT, &path)
        .write_credentials(credentials)
        .body(body.to_string().into_bytes())
        .expect("add_owner_request request")
}

pub fn api_add_owner_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::NO_CONTENT => Ok(()),
        StatusCode::NOT_FOUND => Err(ApiError::NotFound),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(unauthorised_response(&parts.headers)),
        StatusCode::FORBIDDEN => Err(ApiError::Forbidden),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

/// API Docs:
///
/// https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.owner.ex#L125
///
/// https://github.com/hexpm/hex/blob/main/lib/hex/api/package.ex#L19
pub fn api_transfer_owner_request(
    package_name: &str,
    owner: &str,
    credentials: &WriteActionCredentials,
    config: &Config,
) -> http::Request<Vec<u8>> {
    let body = json!({
        "level": OwnerLevel::Full.to_string(),
        "transfer": true,
    });
    let path = format!("packages/{package_name}/owners/{owner}");
    config
        .api_request(Method::PUT, &path)
        .write_credentials(credentials)
        .body(body.to_string().into_bytes())
        .expect("transfer_owner_request request")
}

pub fn api_transfer_owner_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::NO_CONTENT => Ok(()),
        StatusCode::NOT_FOUND => Err(ApiError::NotFound),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(unauthorised_response(&parts.headers)),
        StatusCode::FORBIDDEN => Err(ApiError::Forbidden),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

/// API Docs:
///
/// https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.owner.ex#L139
///
/// https://github.com/hexpm/hex/blob/main/lib/hex/api/package.ex#L28
pub fn api_remove_owner_request(
    package_name: &str,
    owner: &str,
    credentials: &WriteActionCredentials,
    config: &Config,
) -> http::Request<Vec<u8>> {
    let path = format!("packages/{package_name}/owners/{owner}");
    config
        .api_request(Method::DELETE, &path)
        .write_credentials(credentials)
        .body(vec![])
        .expect("remove_owner_request request")
}

pub fn api_remove_owner_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::NO_CONTENT => Ok(()),
        StatusCode::NOT_FOUND => Err(ApiError::NotFound),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(unauthorised_response(&parts.headers)),
        StatusCode::FORBIDDEN => Err(ApiError::Forbidden),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

#[derive(Error, Debug)]
pub enum ApiError {
    #[error(transparent)]
    Json(#[from] serde_json::Error),

    #[error(transparent)]
    Io(#[from] std::io::Error),

    #[error("the rate limit for the Hex API has been exceeded for this IP")]
    RateLimited,

    #[error("invalid authentication credentials")]
    InvalidCredentials,

    #[error("an unexpected response was sent by Hex: {0}: {1}")]
    UnexpectedResponse(StatusCode, String),

    #[error("the given package name {0} is not valid")]
    InvalidPackageNameFormat(String),

    #[error("the payload signature does not match the downloaded payload")]
    IncorrectPayloadSignature,

    #[error(transparent)]
    InvalidProtobuf(#[from] prost::DecodeError),

    #[error("unexpected version format {0}")]
    InvalidVersionFormat(String),

    #[error("resource was not found")]
    NotFound,

    #[error("the version requirement format {0} is not valid")]
    InvalidVersionRequirementFormat(String),

    #[error("the downloaded data did not have the expected checksum")]
    IncorrectChecksum,

    #[error("this account is not authorized for this action")]
    Forbidden,

    #[error("must explicitly express your intention to replace the release")]
    NotReplacing,

    #[error("can only modify a release up to one hour after publication")]
    LateModification,

    #[error("the oauth request wasn't approved in time")]
    OAuthTimeout,

    #[error("the oauth request was rejected")]
    OAuthAccessDenied,

    #[error("the oauth token expired before the request was approved")]
    ExpiredToken,

    #[error("the oauth refresh token was expired, revoked, or already used")]
    OAuthRefreshTokenRejected,

    #[error("the supplied one-time-password was not correct")]
    IncorrectOneTimePassword,
}

impl ApiError {
    fn unexpected_response(status: StatusCode, body: Vec<u8>) -> Self {
        ApiError::UnexpectedResponse(status, String::from_utf8_lossy(&body).to_string())
    }

    /// Returns `true` if the api error is [`NotFound`].
    ///
    /// [`NotFound`]: ApiError::NotFound
    pub fn is_not_found(&self) -> bool {
        matches!(self, Self::NotFound)
    }

    pub fn is_invalid_protobuf(&self) -> bool {
        matches!(self, Self::InvalidProtobuf(_))
    }
}

/// Read a body and ensure it has the given sha256 digest.
fn read_and_check_body(reader: impl std::io::Read, checksum: &[u8]) -> Result<Vec<u8>, ApiError> {
    use std::io::Read;
    let mut reader = BufReader::new(reader);
    let mut context = Context::new(&SHA256);
    let mut buffer = [0; 1024];
    let mut body = Vec::new();

    loop {
        let count = reader.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        let bytes = &buffer[..count];
        context.update(bytes);
        body.extend_from_slice(bytes);
    }

    let digest = context.finish();
    if digest.as_ref() == checksum {
        Ok(body)
    } else {
        Err(ApiError::IncorrectChecksum)
    }
}

fn proto_to_retirement_status(
    status: Option<proto::package::RetirementStatus>,
) -> Option<RetirementStatus> {
    status.map(|stat| RetirementStatus {
        message: stat.message().into(),
        reason: proto_to_retirement_reason(stat.reason()),
    })
}

fn proto_to_retirement_reason(reason: proto::package::RetirementReason) -> RetirementReason {
    use proto::package::RetirementReason::*;
    match reason {
        RetiredOther => RetirementReason::Other,
        RetiredInvalid => RetirementReason::Invalid,
        RetiredSecurity => RetirementReason::Security,
        RetiredDeprecated => RetirementReason::Deprecated,
        RetiredRenamed => RetirementReason::Renamed,
    }
}

fn proto_to_dep(dep: proto::package::Dependency) -> Result<(String, Dependency), ApiError> {
    let app = dep.app;
    let repository = dep.repository;
    let requirement = Range::new(dep.requirement.clone())
        .map_err(|_| ApiError::InvalidVersionFormat(dep.requirement))?;
    Ok((
        dep.package,
        Dependency {
            requirement,
            optional: dep.optional.is_some(),
            app,
            repository,
        },
    ))
}

fn proto_to_release(release: proto::package::Release) -> Result<Release<()>, ApiError> {
    let dependencies = release
        .dependencies
        .clone()
        .into_iter()
        .map(proto_to_dep)
        .collect::<Result<HashMap<_, _>, _>>()?;
    let version = Version::try_from(release.version.as_str())
        .expect("Failed to parse version format from Hex");
    Ok(Release {
        version,
        outer_checksum: release.outer_checksum.unwrap_or_default(),
        retirement_status: proto_to_retirement_status(release.retired),
        requirements: dependencies,
        meta: (),
    })
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Package {
    pub name: String,
    pub repository: String,
    pub releases: Vec<Release<()>>,
}

#[derive(Debug, PartialEq, Eq, Clone, serde::Deserialize)]
pub struct Release<Meta> {
    /// Release version
    pub version: Version,
    /// All dependencies of the release
    pub requirements: HashMap<String, Dependency>,
    /// If set the release is retired, a retired release should only be
    /// resolved if it has already been locked in a project
    pub retirement_status: Option<RetirementStatus>,
    /// sha256 checksum of outer package tarball
    /// required when encoding but optional when decoding
    #[serde(alias = "checksum", deserialize_with = "deserialize_checksum")]
    pub outer_checksum: Vec<u8>,
    /// This is not present in all API endpoints so may be absent sometimes.
    pub meta: Meta,
}

fn deserialize_checksum<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let s: &str = serde::de::Deserialize::deserialize(deserializer)?;
    base16::decode(s).map_err(serde::de::Error::custom)
}

impl<Meta> Release<Meta> {
    pub fn is_retired(&self) -> bool {
        self.retirement_status.is_some()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, serde::Deserialize)]
pub struct ReleaseMeta {
    pub app: String,
    pub build_tools: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Clone, serde::Deserialize)]
pub struct RetirementStatus {
    pub reason: RetirementReason,
    pub message: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RetirementReason {
    Other,
    Invalid,
    Security,
    Deprecated,
    Renamed,
}

impl<'de> serde::Deserialize<'de> for RetirementReason {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s: &str = serde::de::Deserialize::deserialize(deserializer)?;
        match s {
            "other" => Ok(RetirementReason::Other),
            "invalid" => Ok(RetirementReason::Invalid),
            "security" => Ok(RetirementReason::Security),
            "deprecated" => Ok(RetirementReason::Deprecated),
            "renamed" => Ok(RetirementReason::Renamed),
            _ => Err(serde::de::Error::custom("unknown retirement reason type")),
        }
    }
}

impl RetirementReason {
    pub fn to_str(&self) -> &'static str {
        match self {
            RetirementReason::Other => "other",
            RetirementReason::Invalid => "invalid",
            RetirementReason::Security => "security",
            RetirementReason::Deprecated => "deprecated",
            RetirementReason::Renamed => "renamed",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, serde::Deserialize)]
pub struct Dependency {
    /// Version requirement of dependency
    pub requirement: Range,
    /// If true the package is optional and does not need to be resolved
    /// unless another package has specified it as a non-optional dependency.
    pub optional: bool,
    /// If set is the OTP application name of the dependency, if not set the
    /// application name is the same as the package name
    pub app: Option<String>,
    /// If set, the repository where the dependency is located
    pub repository: Option<String>,
}

static USER_AGENT: &str = concat!("Gleam v", env!("CARGO_PKG_VERSION"));

fn validate_package_and_version(package: &str, version: &str) -> Result<(), ApiError> {
    static PACKAGE_PATTERN: OnceLock<Regex> = OnceLock::new();
    static VERSION_PATTERN: OnceLock<Regex> = OnceLock::new();

    let package_pattern = PACKAGE_PATTERN.get_or_init(|| Regex::new(r"^[a-z]\w*$").unwrap());
    let version_pattern =
        VERSION_PATTERN.get_or_init(|| Regex::new(r"^[a-zA-Z-0-9\._-]+$").unwrap());

    if !package_pattern.is_match(package) {
        return Err(ApiError::InvalidPackageNameFormat(package.to_string()));
    }
    if !version_pattern.is_match(version) {
        return Err(ApiError::InvalidVersionFormat(version.to_string()));
    }
    Ok(())
}

// To quote the docs:
//
// > All resources will be signed by the repository's private key.
// > A signed resource is wrapped in a Signed message. The data under
// > the payload field is signed by the signature field.
// >
// > The signature is an (unencoded) RSA signature of the (unencoded)
// > SHA-512 digest of the payload.
//
// https://github.com/hexpm/specifications/blob/master/registry-v2.md#signing
//
fn verify_payload(mut signed: Signed, pem_public_key: &[u8]) -> Result<Vec<u8>, ApiError> {
    let (_, pem) = x509_parser::pem::parse_x509_pem(pem_public_key)
        .map_err(|_| ApiError::IncorrectPayloadSignature)?;
    let (_, spki) = x509_parser::prelude::SubjectPublicKeyInfo::from_der(&pem.contents)
        .map_err(|_| ApiError::IncorrectPayloadSignature)?;
    let payload = std::mem::take(&mut signed.payload);
    let verification = ring::signature::UnparsedPublicKey::new(
        &ring::signature::RSA_PKCS1_2048_8192_SHA512,
        &spki.subject_public_key,
    )
    .verify(payload.as_slice(), signed.signature());

    if verification.is_ok() {
        Ok(payload)
    } else {
        Err(ApiError::IncorrectPayloadSignature)
    }
}

/// Create a request to get the information for a package release.
///
pub fn api_get_package_release_request(
    name: &str,
    version: &str,
    credentials: Option<&Credentials>,
    config: &Config,
) -> http::Request<Vec<u8>> {
    let path = format!("packages/{name}/releases/{version}");
    config
        .api_request(Method::GET, &path)
        .read_credentials(credentials)
        .header("accept", "application/json")
        .body(vec![])
        .expect("get_package_release request")
}

/// Parse a response to get the information for a package release.
///
pub fn api_get_package_release_response(
    response: http::Response<Vec<u8>>,
) -> Result<Release<ReleaseMeta>, ApiError> {
    let (parts, body) = response.into_parts();

    match parts.status {
        StatusCode::OK => Ok(serde_json::from_slice(&body)?),
        StatusCode::NOT_FOUND => Err(ApiError::NotFound),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(unauthorised_response(&parts.headers)),
        StatusCode::FORBIDDEN => Err(ApiError::Forbidden),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

/// Create a device authorisation, kicking off the Hex oauth flow.
pub fn oauth_device_authorisation_request(
    hex_oauth_client_id: &str,
    client_name: &str,
    config: &Config,
) -> http::Request<Vec<u8>> {
    let body = json!({
        "client_id": hex_oauth_client_id,
        "scope": "api:write",
        "name": client_name,
    })
    .to_string()
    .into_bytes();
    config
        .api_request(Method::POST, "oauth/device_authorization")
        .builder
        .body(body)
        .expect("oauth_device_authorisation_request")
}

/// Parse the response of creating a device authorisation, kicking off the Hex oauth flow.
pub fn oauth_device_authorisation_response(
    hex_oauth_client_id: String,
    response: http::Response<Vec<u8>>,
) -> Result<OAuthDeviceAuthorisation, ApiError> {
    let (parts, body) = response.into_parts();

    match parts.status {
        StatusCode::OK => (),
        StatusCode::TOO_MANY_REQUESTS => return Err(ApiError::RateLimited),
        status => return Err(ApiError::unexpected_response(status, body)),
    };

    let data: DeviceAuthorisationResponseBody = serde_json::from_slice(&body)?;
    let poll_interval = Duration::from_secs(data.poll_interval_seconds);
    let verification_uri = data
        .verification_uri_complete
        .unwrap_or(data.verification_uri);

    Ok(OAuthDeviceAuthorisation {
        user_code: data.user_code,
        device_code: data.device_code,
        start_time: Instant::now(),
        client_id: hex_oauth_client_id,
        poll_interval,
        verification_uri,
    })
}

#[derive(Debug)]
pub struct OAuthDeviceAuthorisation {
    /// Show this code to the user for them to match against the one in the Hex UI.
    pub user_code: String,
    /// Send the user to this URI after showing them the code.
    pub verification_uri: String,
    client_id: String,
    device_code: String,
    poll_interval: Duration,
    start_time: Instant,
}

impl OAuthDeviceAuthorisation {
    pub fn poll_token_request(&self, config: &Config) -> http::Request<Vec<u8>> {
        let body = json!({
            "grant_type": "urn:ietf:params:oauth:grant-type:device_code",
            "client_id": &self.client_id,
            "device_code": &self.device_code,
        })
        .to_string()
        .into_bytes();
        config
            .api_request(Method::POST, "oauth/token")
            .builder
            .body(body)
            .expect("poll_token_request")
    }

    pub fn poll_token_response(
        &mut self,
        response: http::Response<Vec<u8>>,
    ) -> Result<PollStep, ApiError> {
        if self.start_time.elapsed() > Duration::from_mins(10) {
            return Err(ApiError::OAuthTimeout);
        }

        let (parts, body) = response.into_parts();

        match parts.status {
            StatusCode::OK | StatusCode::BAD_REQUEST | StatusCode::FORBIDDEN => (),
            status => return Err(ApiError::unexpected_response(status, body)),
        };

        let data: PollResponseBody = serde_json::from_slice(&body)?;
        match data.into_result() {
            Ok(tokens) => Ok(PollStep::Done(tokens)),

            Err(PollResponseBodyError::AuthorizationPending) => {
                Ok(PollStep::SleepThenPollAgain(self.poll_interval.clone()))
            }

            Err(PollResponseBodyError::SlowDown) => {
                let max_interval = Duration::from_secs(30);
                self.poll_interval = self.poll_interval.saturating_mul(2).min(max_interval);
                Ok(PollStep::SleepThenPollAgain(self.poll_interval.clone()))
            }

            Err(PollResponseBodyError::AccessDenied) => Err(ApiError::OAuthAccessDenied),

            Err(PollResponseBodyError::ExpiredToken) => Err(ApiError::ExpiredToken),
        }
    }
}

#[derive(Debug)]
pub enum PollStep {
    Done(OAuthTokens),
    SleepThenPollAgain(Duration),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OAuthTokens {
    pub access_token: EcoString,
    pub refresh_token: EcoString,
}

impl OAuthTokens {
    pub fn as_credentials(&self) -> Credentials {
        Credentials::OAuthAccessToken(self.access_token.clone())
    }
}

#[derive(Debug, Deserialize)]
struct DeviceAuthorisationResponseBody {
    #[serde(default = "default_poll_interval_seconds", rename = "interval")]
    poll_interval_seconds: u64,
    device_code: String,
    user_code: String,
    verification_uri: String,
    verification_uri_complete: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum PollResponseBody {
    Success {
        access_token: EcoString,
        refresh_token: EcoString,
    },
    Fail {
        error: PollResponseBodyError,
    },
}

impl PollResponseBody {
    pub fn into_result(self) -> Result<OAuthTokens, PollResponseBodyError> {
        match self {
            PollResponseBody::Success {
                access_token,
                refresh_token,
            } => Ok(OAuthTokens {
                access_token,
                refresh_token,
            }),
            PollResponseBody::Fail { error } => Err(error),
        }
    }
}

#[derive(Debug, Deserialize)]
enum PollResponseBodyError {
    #[serde(rename = "authorization_pending")]
    AuthorizationPending,
    #[serde(rename = "slow_down")]
    SlowDown,
    #[serde(rename = "access_denied")]
    AccessDenied,
    #[serde(rename = "expired_token")]
    ExpiredToken,
}

fn default_poll_interval_seconds() -> u64 {
    5
}
/// Use a refresh tokent to get an access token that can be used to
/// authenticate with the API.
pub fn oauth_refresh_token_request(
    hex_oauth_client_id: &str,
    refresh_token: &str,
    config: &Config,
) -> http::Request<Vec<u8>> {
    let body = json!({
        "grant_type": "refresh_token",
        "client_id": hex_oauth_client_id,
        "refresh_token": refresh_token,
    })
    .to_string()
    .into_bytes();
    config
        .api_request(Method::POST, "oauth/token")
        .builder
        .body(body)
        .expect("oauth_refresh_token_request")
}

pub fn oauth_refresh_token_response(
    response: http::Response<Vec<u8>>,
) -> Result<OAuthTokens, ApiError> {
    let (parts, body) = response.into_parts();

    match parts.status {
        StatusCode::OK => (),
        StatusCode::TOO_MANY_REQUESTS => return Err(ApiError::RateLimited),
        StatusCode::BAD_REQUEST => return Err(ApiError::OAuthRefreshTokenRejected),
        status => return Err(ApiError::unexpected_response(status, body)),
    };

    Ok(serde_json::from_slice(&body)?)
}

/// Get information about the currently authenticated user.
pub fn get_me_request(credentials: &Credentials, config: &Config) -> http::Request<Vec<u8>> {
    config
        .api_request(Method::GET, "users/me")
        .read_credentials(Some(credentials))
        .body(vec![])
        .expect("me_request")
}

/// Get information about the currently authenticated user.
pub fn get_me_response(response: http::Response<Vec<u8>>) -> Result<Me, ApiError> {
    let (parts, body) = response.into_parts();

    match parts.status {
        StatusCode::OK => (),
        status => return Err(ApiError::unexpected_response(status, body)),
    };

    Ok(serde_json::from_slice(&body)?)
}

#[derive(Debug, PartialEq, Eq, Clone, serde::Deserialize)]
pub struct Me {
    pub username: String,
}

/// Create a request to revoke an OAuth token.
///
pub fn revoke_oauth_token_by_hash_request(
    refresh_token_hash: &str,
    credentials: &WriteActionCredentials,
    config: &Config,
) -> http::Request<Vec<u8>> {
    let body = json!({
        "token_hash": refresh_token_hash,
    })
    .to_string()
    .into_bytes();
    config
        .api_request(Method::GET, &format!("oauth/revoke_by_hash"))
        .write_credentials(credentials)
        .header("accept", "application/json")
        .body(body)
        .expect("api_get_package_release_request request")
}

/// Parse a response to revoke an OAuth token.
///
pub fn revoke_oauth_token_by_hash_response(
    response: http::Response<Vec<u8>>,
) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::OK => (),
        status => return Err(ApiError::unexpected_response(status, body)),
    };
    Ok(())
}
