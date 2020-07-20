mod proto;

#[cfg(test)]
mod tests;

use crate::proto::{signed::Signed, versions::Versions};
use async_trait::async_trait;
use bytes::buf::ext::BufExt;
use flate2::read::GzDecoder;
use lazy_static::lazy_static;
use regex::Regex;
use reqwest::StatusCode;
use serde::Deserialize;
use serde_json::json;
use std::collections::HashMap;
use thiserror::Error;

#[async_trait]
pub trait Client {
    fn http_client(&self) -> reqwest::Client;
    fn api_base_url(&self) -> &url::Url;
    fn repository_base_url(&self) -> &url::Url;

    /// Authenticate with the Hex API using a username and password in order
    /// to get an API token, enabling accessing of more APIs and raising the
    /// rate limit.
    async fn authenticate(
        &self,
        username: &str,
        password: &str,
        token_name: &str,
    ) -> Result<AuthenticatedClient, AuthenticateError> {
        let body = json!({
            "name": token_name,
            "permissions": [{
                "domain": "api",
                "resource": "write",
            }],
        });

        let response = self
            .http_client()
            .post(self.api_base_url().join("keys").unwrap())
            .basic_auth(username, Some(password))
            .json(&body)
            .send()
            .await
            .map_err(AuthenticateError::Http)?;

        match response.status() {
            StatusCode::CREATED => {
                let body: AuthenticateResponseCreated =
                    response.json().await.map_err(AuthenticateError::Http)?;
                Ok(AuthenticatedClient {
                    repository_base: self.repository_base_url().clone(),
                    api_base: self.api_base_url().clone(),
                    api_token: body.secret,
                })
            }

            StatusCode::TOO_MANY_REQUESTS => Err(AuthenticateError::RateLimited),

            StatusCode::UNAUTHORIZED => Err(AuthenticateError::InvalidCredentials),

            status => Err(AuthenticateError::UnexpectedResponse(
                status,
                response.text().await.unwrap_or_default(),
            )),
        }
    }

    /// Get the names and versions of all of the packages on the package registry.
    ///
    async fn get_repository_versions(
        &self,
        public_key: &[u8],
    ) -> Result<HashMap<String, Vec<String>>, GetRepositoryVersionsError> {
        let response = self
            .http_client()
            .get(self.repository_base_url().join("versions").unwrap())
            .send()
            .await
            .map_err(GetRepositoryVersionsError::Http)?;

        match response.status() {
            StatusCode::OK => (),
            status => {
                return Err(GetRepositoryVersionsError::UnexpectedResponse(
                    status,
                    response.text().await.unwrap_or_default(),
                ));
            }
        };

        let body = response
            .bytes()
            .await
            .map_err(GetRepositoryVersionsError::Http)?
            .reader();

        let mut body = GzDecoder::new(body);
        let signed = protobuf::parse_from_reader::<Signed>(&mut body)
            .map_err(GetRepositoryVersionsError::DecodeFailed)?;

        let payload = verify_payload(signed, public_key)
            .map_err(|_| GetRepositoryVersionsError::IncorrectPayloadSignature)?;

        let versions = protobuf::parse_from_bytes::<Versions>(&payload)
            .map_err(GetRepositoryVersionsError::DecodeFailed)?
            .take_packages()
            .into_iter()
            .map(|mut n| (n.take_name(), n.take_versions().into_vec()))
            .collect();

        Ok(versions)
    }
}

#[derive(Error, Debug)]
pub enum GetRepositoryVersionsError {
    #[error(transparent)]
    Http(#[from] reqwest::Error),

    #[error("an unexpected response was sent by Hex")]
    UnexpectedResponse(StatusCode, String),

    #[error("the payload signature does not match the downloaded payload")]
    IncorrectPayloadSignature,

    #[error(transparent)]
    DecodeFailed(#[from] protobuf::ProtobufError),
}

static USER_AGENT: &str = concat!(env!("CARGO_PKG_NAME"), " (", env!("CARGO_PKG_VERSION"), ")");

#[derive(Debug)]
pub struct UnauthenticatedClient {
    pub api_base: url::Url,
    pub repository_base: url::Url,
}

impl Client for UnauthenticatedClient {
    fn http_client(&self) -> reqwest::Client {
        let mut headers = http::header::HeaderMap::new();
        headers.insert("Accept", "application/json".parse().unwrap());

        reqwest::ClientBuilder::new()
            .user_agent(USER_AGENT)
            .default_headers(headers)
            .build()
            .expect("failed to build API client")
    }

    fn api_base_url(&self) -> &url::Url {
        &self.api_base
    }

    fn repository_base_url(&self) -> &url::Url {
        &self.repository_base
    }
}

impl UnauthenticatedClient {
    pub fn new() -> Self {
        Self {
            api_base: url::Url::parse("https://hex.pm/api/").unwrap(),
            repository_base: url::Url::parse("https://repo.hex.pm/").unwrap(),
        }
    }
}

#[derive(Error, Debug)]
pub enum AuthenticateError {
    #[error(transparent)]
    Http(#[from] reqwest::Error),

    #[error("the rate limit for the Hex API has been exceeded for this IP")]
    RateLimited,

    #[error("invalid username and password combination")]
    InvalidCredentials,

    #[error("an unexpected response was sent by Hex")]
    UnexpectedResponse(StatusCode, String),
}

#[derive(Debug, Deserialize)]
struct AuthenticateResponseCreated {
    secret: String,
}

#[derive(Debug)]
pub struct AuthenticatedClient {
    pub api_base: url::Url,
    pub repository_base: url::Url,
    pub api_token: String,
}

impl Client for AuthenticatedClient {
    fn http_client(&self) -> reqwest::Client {
        let mut headers = http::header::HeaderMap::new();
        headers.insert("Authorization", self.api_token.parse().unwrap());
        headers.insert("Accept", "application/json".parse().unwrap());

        reqwest::ClientBuilder::new()
            .user_agent(USER_AGENT)
            .default_headers(headers)
            .build()
            .expect("failed to build API client")
    }

    fn api_base_url(&self) -> &url::Url {
        &self.api_base
    }

    fn repository_base_url(&self) -> &url::Url {
        &self.repository_base
    }
}

impl AuthenticatedClient {
    pub fn new(api_token: String) -> Self {
        Self {
            api_base: url::Url::parse("https://hex.pm/api/").unwrap(),
            repository_base: url::Url::parse("https://repo.hex.pm/").unwrap(),
            api_token,
        }
    }

    pub async fn remove_docs<'a>(
        &self,
        package_name: &'a str,
        version: &'a str,
    ) -> Result<(), RemoveDocsError<'a>> {
        validate_package_and_version(package_name, version)
            .map_err(|_| RemoveDocsError::BadPackage(package_name, version))?;

        let url = self
            .api_base
            .join(format!("packages/{}/releases/{}/docs", package_name, version).as_str())
            .expect("building remove_docs url");

        let response = self
            .http_client()
            .delete(url.to_string().as_str())
            .send()
            .await
            .map_err(RemoveDocsError::Http)?;

        match response.status() {
            StatusCode::NO_CONTENT => Ok(()),
            StatusCode::NOT_FOUND => Err(RemoveDocsError::NotFound(package_name, version)),
            StatusCode::TOO_MANY_REQUESTS => Err(RemoveDocsError::RateLimited),
            StatusCode::UNAUTHORIZED => Err(RemoveDocsError::InvalidApiKey),
            StatusCode::FORBIDDEN => Err(RemoveDocsError::Forbidden),
            status => Err(RemoveDocsError::UnexpectedResponse(
                status,
                response.text().await.unwrap_or_default(),
            )),
        }
    }

    pub async fn publish_docs<'a>(
        &self,
        package_name: &'a str,
        version: &'a str,
        gzipped_tarball: bytes::Bytes,
    ) -> Result<(), PublishDocsError<'a>> {
        validate_package_and_version(package_name, version)
            .map_err(|_| PublishDocsError::BadPackage(package_name, version))?;

        let url = self
            .api_base
            .join(format!("packages/{}/releases/{}/docs", package_name, version).as_str())
            .expect("building remove_docs url");

        let response = self
            .http_client()
            .post(url.to_string().as_str())
            .body(gzipped_tarball)
            .send()
            .await
            .map_err(PublishDocsError::Http)?;

        match response.status() {
            StatusCode::CREATED => Ok(()),
            StatusCode::NOT_FOUND => Err(PublishDocsError::NotFound(package_name, version)),
            StatusCode::TOO_MANY_REQUESTS => Err(PublishDocsError::RateLimited),
            StatusCode::UNAUTHORIZED => Err(PublishDocsError::InvalidApiKey),
            StatusCode::FORBIDDEN => Err(PublishDocsError::Forbidden),
            status => Err(PublishDocsError::UnexpectedResponse(
                status,
                response.text().await.unwrap_or_default(),
            )),
        }
    }
}

#[derive(Error, Debug)]
pub enum RemoveDocsError<'a> {
    #[error(transparent)]
    Http(#[from] reqwest::Error),

    #[error("the given package name and version {0} {1} are not valid")]
    BadPackage(&'a str, &'a str),

    #[error("could not find package {0} with version {1}")]
    NotFound(&'a str, &'a str),

    #[error("the rate limit for the Hex API has been exceeded for this IP")]
    RateLimited,

    #[error("the given API key was not valid")]
    InvalidApiKey,

    #[error("this account is not authorized for this action")]
    Forbidden,

    #[error("an unexpected response was sent by Hex")]
    UnexpectedResponse(StatusCode, String),
}

#[derive(Error, Debug)]
pub enum PublishDocsError<'a> {
    #[error(transparent)]
    Http(#[from] reqwest::Error),

    #[error("the given package name and version {0} {1} are not valid")]
    BadPackage(&'a str, &'a str),

    #[error("could not find package {0} with version {1}")]
    NotFound(&'a str, &'a str),

    #[error("the rate limit for the Hex API has been exceeded for this IP")]
    RateLimited,

    #[error("the given API key was not valid")]
    InvalidApiKey,

    #[error("this account is not authorized for this action")]
    Forbidden,

    #[error("an unexpected response was sent by Hex")]
    UnexpectedResponse(StatusCode, String),
}

fn validate_package_and_version(package: &str, version: &str) -> Result<(), ()> {
    lazy_static! {
        static ref PACKAGE_PATTERN: Regex = Regex::new(r#"^[a-z_-]+$"#).unwrap();
        static ref VERSION_PATTERN: Regex = Regex::new(r#"^[a-zA-Z-0-9\._-]+$"#).unwrap();
    }
    if !PACKAGE_PATTERN.is_match(package) {
        return Err(());
    }
    if !VERSION_PATTERN.is_match(version) {
        return Err(());
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
fn verify_payload(mut signed: Signed, pem_public_key: &[u8]) -> Result<Vec<u8>, ()> {
    // TODO: convert the public key to the right format? ...Maybe? I've tried to do this quite a
    // few different ways now but I've not had any success.
    // https://twitter.com/obmarg/status/1274722498286956545

    let (_, pem) = x509_parser::pem::pem_to_der(pem_public_key).unwrap();
    let (_, spki) = x509_parser::parse_subject_public_key_info(&pem.contents).unwrap();
    let payload = signed.take_payload();
    let verification = ring::signature::UnparsedPublicKey::new(
        &ring::signature::RSA_PKCS1_2048_8192_SHA512,
        &spki.subject_public_key,
    )
    .verify(payload.as_slice(), signed.get_signature());

    if verification.is_ok() {
        Ok(payload)
    } else {
        Err(())
    }
}
