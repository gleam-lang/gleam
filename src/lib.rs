#[cfg(test)]
mod tests;

use async_trait::async_trait;
use lazy_static::lazy_static;
use regex::Regex;
use reqwest::StatusCode;
use serde::Deserialize;
use serde_json::json;
use thiserror::Error;

#[async_trait]
pub trait Client {
    fn http_client(&self) -> reqwest::Client;
    fn base_url(&self) -> &url::Url;

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
            .post(self.base_url().join("keys").unwrap())
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
                    api_base_url: self.base_url().clone(),
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
}

static USER_AGENT: &str = concat!(env!("CARGO_PKG_NAME"), " (", env!("CARGO_PKG_VERSION"), ")");

#[derive(Debug)]
pub struct UnauthenticatedClient {
    pub api_base_url: url::Url,
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

    fn base_url(&self) -> &url::Url {
        &self.api_base_url
    }
}

impl UnauthenticatedClient {
    pub fn new() -> Self {
        Self {
            api_base_url: url::Url::parse("https://hex.pm/api/").unwrap(),
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
    pub api_base_url: url::Url,
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

    fn base_url(&self) -> &url::Url {
        &self.api_base_url
    }
}

impl AuthenticatedClient {
    pub fn new(api_token: String) -> Self {
        Self {
            api_base_url: url::Url::parse("https://hex.pm/api/").unwrap(),
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
            .api_base_url
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
            .api_base_url
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
