mod proto;

#[cfg(test)]
mod tests;

pub mod version;

use crate::proto::{signed::Signed, versions::Versions};
use bytes::buf::Buf;
use flate2::read::GzDecoder;
use http::{Method, StatusCode};
use lazy_static::lazy_static;
use protobuf::Message;
use regex::Regex;
use ring::digest::{Context, SHA256};
use serde::Deserialize;
use serde_json::json;
use std::{collections::HashMap, convert::TryFrom, convert::TryInto, io::BufReader};
use thiserror::Error;
use version::{Range, Version};

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

    fn api_request(
        &self,
        method: http::Method,
        path_suffix: &str,
        api_key: Option<&str>,
    ) -> http::request::Builder {
        make_request(self.api_base.clone(), method, path_suffix, api_key)
            .header("content-type", "application/json")
            .header("accept", "application/json")
    }

    fn repository_request(
        &self,
        method: http::Method,
        path_suffix: &str,
        api_key: Option<&str>,
    ) -> http::request::Builder {
        make_request(self.repository_base.clone(), method, path_suffix, api_key)
    }
}

fn make_request(
    base: http::Uri,
    method: http::Method,
    path_suffix: &str,
    api_key: Option<&str>,
) -> http::request::Builder {
    let mut parts = base.into_parts();
    parts.path_and_query = Some(
        match parts.path_and_query {
            Some(path) => format!("{}{}", path, path_suffix).try_into(),
            None => path_suffix.try_into(),
        }
        .expect("api_uri path"),
    );
    let uri = http::Uri::from_parts(parts).expect("api_uri building");
    let mut builder = http::Request::builder()
        .method(method)
        .uri(uri)
        .header("user-agent", USER_AGENT);
    if let Some(key) = api_key {
        builder = builder.header("authorization", key);
    }
    builder
}

/// Create a request that creates a Hex API key.
pub fn create_api_key_request(
    username: &str,
    password: &str,
    key_name: &str,
    config: &Config,
) -> http::Request<Vec<u8>> {
    let body = json!({
        "name": key_name,
        "permissions": [{
            "domain": "api",
            "resource": "write",
        }],
    });
    let creds = http_auth_basic::Credentials::new(username, password).as_http_header();
    config
        .api_request(Method::POST, "keys", None)
        .header("authorization", creds)
        .body(body.to_string().into_bytes())
        .expect("create_api_key_request request")
}

/// Parses a request that creates a Hex API key.
pub fn create_api_key_response(response: http::Response<Vec<u8>>) -> Result<String, ApiError> {
    #[derive(Deserialize)]
    struct Resp {
        secret: String,
    }
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::CREATED => Ok(serde_json::from_slice::<Resp>(&body)?.secret),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(ApiError::InvalidCredentials),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

/// Create a request that deletes an Hex API key.
pub fn remove_api_key_request(
    name_of_key_to_delete: &str,
    api_key: &str,
    config: &Config,
) -> http::Request<Vec<u8>> {
    config
        .api_request(
            Method::DELETE,
            &format!("keys/{}", name_of_key_to_delete),
            Some(api_key),
        )
        .body(vec![])
        .expect("get_package_tarball_request request")
}

/// Parses a request that deleted a Hex API key.
pub fn remove_api_key_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::NO_CONTENT | StatusCode::OK => Ok(()),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(ApiError::InvalidCredentials),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

/// Create a request that get the names and versions of all of the packages on
/// the package registry.
///
pub fn get_repository_versions_request(
    api_key: Option<&str>,
    config: &Config,
) -> http::Request<Vec<u8>> {
    config
        .repository_request(Method::GET, "versions", api_key)
        .header("accept", "application/json")
        .body(vec![])
        .expect("create_api_key_request request")
}

/// Parse a request that get the names and versions of all of the packages on
/// the package registry.
///
pub fn get_repository_versions_response(
    response: http::Response<Vec<u8>>,
    public_key: &[u8],
) -> Result<HashMap<String, Vec<Version>>, ApiError> {
    let (parts, body) = response.into_parts();

    match parts.status {
        StatusCode::OK => (),
        status => return Err(ApiError::unexpected_response(status, body)),
    };

    let mut body = GzDecoder::new(body.reader());
    let signed = Signed::parse_from_reader(&mut body)?;

    let payload =
        verify_payload(signed, public_key).map_err(|_| ApiError::IncorrectPayloadSignature)?;

    let versions = Versions::parse_from_bytes(&payload)?
        .take_packages()
        .into_iter()
        .map(|mut n| {
            let parse_version = |v: &str| {
                let err = |_| ApiError::InvalidVersionFormat(v.to_string());
                Version::parse(v).map_err(err)
            };
            let versions = n
                .take_versions()
                .into_iter()
                .map(|v| parse_version(v.as_str()))
                .collect::<Result<Vec<Version>, ApiError>>()?;
            Ok((n.take_name(), versions))
        })
        .collect::<Result<HashMap<_, _>, ApiError>>()?;

    Ok(versions)
}

/// Create a request to get the information for a package in the repository.
///
pub fn get_package_request(
    name: &str,
    api_key: Option<&str>,
    config: &Config,
) -> http::Request<Vec<u8>> {
    config
        .repository_request(Method::GET, &format!("packages/{}", name), api_key)
        .header("accept", "application/json")
        .body(vec![])
        .expect("get_package_request request")
}

/// Parse a response to get the information for a package in the repository.
///
pub fn get_package_response(
    response: http::Response<Vec<u8>>,
    public_key: &[u8],
) -> Result<Package, ApiError> {
    let (parts, body) = response.into_parts();

    match parts.status {
        StatusCode::OK => (),
        StatusCode::FORBIDDEN => return Err(ApiError::NotFound), // Oddly this is the not-found code
        status => {
            return Err(ApiError::unexpected_response(status, body));
        }
    };

    let mut body = GzDecoder::new(body.reader());
    let signed = Signed::parse_from_reader(&mut body)?;

    let payload =
        verify_payload(signed, public_key).map_err(|_| ApiError::IncorrectPayloadSignature)?;

    let mut package = proto::package::Package::parse_from_bytes(&payload)?;
    let releases = package
        .take_releases()
        .into_iter()
        .map(proto_to_release)
        .collect::<Result<Vec<_>, _>>()?;
    let package = Package {
        name: package.take_name(),
        repository: package.take_repository(),
        releases,
    };

    Ok(package)
}

/// Create a request to download a version of a package as a tarball
///
pub fn get_package_tarball_request(
    name: &str,
    version: &str,
    api_key: Option<&str>,
    config: &Config,
) -> http::Request<Vec<u8>> {
    config
        .repository_request(
            Method::GET,
            &format!("tarballs/{}-{}.tar", name, version),
            api_key,
        )
        .header("accept", "application/x-tar")
        .body(vec![])
        .expect("get_package_tarball_request request")
}

/// Parse a response to download a version of a package as a tarball
///
pub fn get_package_tarball_response(
    response: http::Response<Vec<u8>>,
    checksum: &[u8],
) -> Result<Vec<u8>, ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::OK => (),
        StatusCode::FORBIDDEN => return Err(ApiError::NotFound),
        status => {
            return Err(ApiError::unexpected_response(status, body));
        }
    };
    let body = read_and_check_body(body.reader(), checksum)?;
    Ok(body)
}

pub fn remove_docs_request(
    package_name: &str,
    version: &str,
    api_key: &str,
    config: &Config,
) -> Result<http::Request<Vec<u8>>, ApiError> {
    validate_package_and_version(package_name, version)?;

    Ok(config
        .api_request(
            Method::DELETE,
            &format!("packages/{}/releases/{}/docs", package_name, version),
            Some(api_key),
        )
        .body(vec![])
        .expect("get_package_tarball_request request"))
}

pub fn remove_docs_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::NO_CONTENT => Ok(()),
        StatusCode::NOT_FOUND => Err(ApiError::NotFound),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(ApiError::InvalidApiKey),
        StatusCode::FORBIDDEN => Err(ApiError::Forbidden),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

pub fn publish_docs_request(
    package_name: &str,
    version: &str,
    gzipped_tarball: Vec<u8>,
    api_key: &str,
    config: &Config,
) -> Result<http::Request<Vec<u8>>, ApiError> {
    validate_package_and_version(package_name, version)?;

    Ok(config
        .api_request(
            Method::POST,
            &format!("packages/{}/releases/{}/docs", package_name, version),
            Some(api_key),
        )
        .header("content-encoding", "x-gzip")
        .header("content-type", "application/x-tar")
        .body(gzipped_tarball)
        .expect("publish_docs_request request"))
}

pub fn publish_docs_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::CREATED => Ok(()),
        StatusCode::NOT_FOUND => Err(ApiError::NotFound),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(ApiError::InvalidApiKey),
        StatusCode::FORBIDDEN => Err(ApiError::Forbidden),
        status => Err(ApiError::unexpected_response(status, body)),
    }
}

pub fn publish_package_request(
    release_tarball: Vec<u8>,
    api_key: &str,
    config: &Config,
) -> http::Request<Vec<u8>> {
    // TODO: do all the package tarball construction
    config
        .api_request(Method::POST, "publish", Some(api_key))
        .header("content-type", "application/x-tar")
        .body(release_tarball)
        .expect("publish_package_request request")
}

pub fn publish_package_response(response: http::Response<Vec<u8>>) -> Result<(), ApiError> {
    // TODO: return data from body
    let (parts, body) = response.into_parts();
    match parts.status {
        StatusCode::CREATED => Ok(()),
        StatusCode::NOT_FOUND => Err(ApiError::NotFound),
        StatusCode::TOO_MANY_REQUESTS => Err(ApiError::RateLimited),
        StatusCode::UNAUTHORIZED => Err(ApiError::InvalidApiKey),
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

    #[error("invalid username and password combination")]
    InvalidCredentials,

    #[error("an unexpected response was sent by Hex: {0}: {1}")]
    UnexpectedResponse(StatusCode, String),

    #[error("the given package name {0} is not valid")]
    InvalidPackageNameFormat(String),

    #[error("the payload signature does not match the downloaded payload")]
    IncorrectPayloadSignature,

    #[error(transparent)]
    InvalidProtobuf(#[from] protobuf::ProtobufError),

    #[error("unexpected version format {0}")]
    InvalidVersionFormat(String),

    #[error("resource was not found")]
    NotFound,

    #[error("the version requirement format {0} is not valid")]
    InvalidVersionRequirementFormat(String),

    #[error("the downloaded data did not have the expected checksum")]
    IncorrectChecksum,

    #[error("the given API key was not valid")]
    InvalidApiKey,

    #[error("this account is not authorized for this action")]
    Forbidden,
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
    mut status: proto::package::RetirementStatus,
) -> Option<RetirementStatus> {
    if status.has_reason() {
        Some(RetirementStatus {
            message: status.take_message(),
            reason: proto_to_retirement_reason(status.get_reason()),
        })
    } else {
        None
    }
}

fn proto_to_retirement_reason(reason: proto::package::RetirementReason) -> RetirementReason {
    use proto::package::RetirementReason::*;
    match reason {
        RETIRED_OTHER => RetirementReason::Other,
        RETIRED_INVALID => RetirementReason::Invalid,
        RETIRED_SECURITY => RetirementReason::Security,
        RETIRED_DEPRECATED => RetirementReason::Deprecated,
        RETIRED_RENAMED => RetirementReason::Renamed,
    }
}

fn proto_to_dep(mut dep: proto::package::Dependency) -> Result<Dependency, ApiError> {
    let app = if dep.has_app() {
        Some(dep.take_app())
    } else {
        None
    };
    let repository = if dep.has_repository() {
        Some(dep.take_repository())
    } else {
        None
    };
    let requirement = Range::new(dep.take_requirement());
    Ok(Dependency {
        package: dep.take_package(),
        requirement,
        optional: dep.has_optional(),
        app,
        repository,
    })
}

fn proto_to_release(mut release: proto::package::Release) -> Result<Release, ApiError> {
    let dependencies = release
        .take_dependencies()
        .into_iter()
        .map(proto_to_dep)
        .collect::<Result<Vec<_>, _>>()?;
    let version =
        Version::try_from(release.get_version()).expect("Failed to parse version format from Hex");
    Ok(Release {
        version,
        outer_checksum: release.take_outer_checksum(),
        retirement_status: proto_to_retirement_status(release.take_retired()),
        dependencies,
    })
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Package {
    pub name: String,
    pub repository: String,
    pub releases: Vec<Release>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Release {
    /// Release version
    pub version: Version,
    /// All dependencies of the release
    pub dependencies: Vec<Dependency>,
    /// If set the release is retired, a retired release should only be
    /// resolved if it has already been locked in a project
    pub retirement_status: Option<RetirementStatus>,
    /// sha256 checksum of outer package tarball
    /// required when encoding but optional when decoding
    pub outer_checksum: Vec<u8>,
}

impl Release {
    pub fn is_retired(&self) -> bool {
        self.retirement_status.is_some()
    }

    fn is_pre(&self) -> bool {
        self.version.is_pre()
    }

    fn is_active(&self) -> bool {
        self.retirement_status.is_none()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Dependency {
    /// Package name of dependency
    pub package: String,
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

static USER_AGENT: &str = concat!(env!("CARGO_PKG_NAME"), " (", env!("CARGO_PKG_VERSION"), ")");

fn validate_package_and_version(package: &str, version: &str) -> Result<(), ApiError> {
    lazy_static! {
        static ref PACKAGE_PATTERN: Regex = Regex::new(r#"^[a-z_-]+$"#).unwrap();
        static ref VERSION_PATTERN: Regex = Regex::new(r#"^[a-zA-Z-0-9\._-]+$"#).unwrap();
    }
    if !PACKAGE_PATTERN.is_match(package) {
        return Err(ApiError::InvalidPackageNameFormat(package.to_string()));
    }
    if !VERSION_PATTERN.is_match(version) {
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
    let payload = signed.take_payload();
    let verification = ring::signature::UnparsedPublicKey::new(
        &ring::signature::RSA_PKCS1_2048_8192_SHA512,
        &spki.subject_public_key,
    )
    .verify(payload.as_slice(), signed.get_signature());

    if verification.is_ok() {
        Ok(payload)
    } else {
        Err(ApiError::IncorrectPayloadSignature)
    }
}
