#[cfg(test)]
mod tests;

use reqwest::StatusCode;
use serde::Deserialize;
use serde_json::json;
use thiserror::Error;

static USER_AGENT: &str = concat!(env!("CARGO_PKG_NAME"), " (", env!("CARGO_PKG_VERSION"), ")");

#[derive(Debug)]
pub struct UnauthenticatedClient {
    pub api_base_url: url::Url,
}

impl UnauthenticatedClient {
    pub fn new() -> Self {
        Self {
            api_base_url: url::Url::parse("https://hex.pm/api").unwrap(),
        }
    }

    fn http_client(&self) -> reqwest::Client {
        reqwest::ClientBuilder::new()
            .user_agent(USER_AGENT)
            .build()
            .expect("failed to build API client")
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

        let response = self
            .http_client()
            .post(self.api_base_url.join("keys").unwrap())
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
