#[cfg(test)]
mod tests;

use reqwest::StatusCode;
use serde::Deserialize;
use serde_json::json;
use thiserror::Error;

pub trait Client {
    fn http_client(&self) -> reqwest::Client;
    fn base_url(&self) -> &url::Url;
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

    /// Authenticate with the Hex API using a username and password in order
    /// to get an API token, enabling accessing of more APIs and raising the
    /// rate limit.
    pub async fn authenticate(
        self,
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

        let response = dbg!(self
            .http_client()
            .post(self.api_base_url.join("keys").unwrap()))
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
                    api_base_url: self.api_base_url,
                    api_token: body.secret,
                })
            }

            StatusCode::TOO_MANY_REQUESTS => Err(AuthenticateError::RateLimited),

            StatusCode::UNAUTHORIZED => Err(AuthenticateError::InvalidCredentials),

            code => panic!("code: {}, resp: {:?}", code, response.text().await.unwrap()),
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
        let url = self
            .api_base_url
            .join(format!("packages/{}/releases/{}/docs", package_name, version).as_str())
            .map_err(|_| RemoveDocsError::BadUrl(package_name, version))?;

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
            code => panic!("code: {}, resp: {:?}", code, response.text().await.unwrap()),
        }
    }
}

#[derive(Error, Debug)]
pub enum RemoveDocsError<'a> {
    #[error(transparent)]
    Http(#[from] reqwest::Error),

    #[error("the given package name and version {0} {1} are not URL safe")]
    BadUrl(&'a str, &'a str),

    #[error("could not find package {0} with version {1}")]
    NotFound(&'a str, &'a str),

    #[error("the rate limit for the Hex API has been exceeded for this IP")]
    RateLimited,

    #[error("the given API key was not valid")]
    InvalidApiKey,

    #[error("this account is not authorized for this action")]
    Forbidden,
}
