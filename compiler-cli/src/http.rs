use std::convert::TryInto;
use std::sync::OnceLock;

use async_trait::async_trait;
use gleam_core::{Error, Result};
use http::{Request, Response};

static REQWEST_CLIENT: OnceLock<reqwest::Client> = OnceLock::new();
static CERTS_ENV_VAR: &str = "GLEAM_CACERTS_PATH";

#[derive(Debug)]
pub struct HttpClient;

impl HttpClient {
    pub fn new() -> Self {
        Self
    }

    pub fn boxed() -> Box<Self> {
        Box::new(Self::new())
    }
}

#[async_trait]
impl gleam_core::io::HttpClient for HttpClient {
    async fn send(&self, request: Request<Vec<u8>>) -> Result<Response<Vec<u8>>> {
        let request = request
            .try_into()
            .expect("Unable to convert HTTP request for use by reqwest library");
        let mut response = REQWEST_CLIENT
            .get_or_init(init_client)
            .execute(request)
            .await
            .map_err(Error::http)?;
        let mut builder = Response::builder()
            .status(response.status())
            .version(response.version());
        if let Some(headers) = builder.headers_mut() {
            std::mem::swap(headers, response.headers_mut());
        }
        builder
            .body(response.bytes().await.map_err(Error::http)?.to_vec())
            .map_err(Error::http)
    }
}

fn init_client() -> reqwest::Client {
    if let Some(cert) = get_certificate() {
        return reqwest::Client::builder()
            .add_root_certificate(cert)
            .build()
            .expect("Unable to initialize a reqwest HTTP client");
    } else {
        return reqwest::Client::new();
    }
}

fn get_certificate() -> Option<reqwest::Certificate> {
    match std::env::var(CERTS_ENV_VAR) {
        Ok(certs_path) => {
            let data = std::fs::read(certs_path).expect(&format!(
                "Unable to read certs file set as `{}`",
                CERTS_ENV_VAR
            ));
            let cert = reqwest::Certificate::from_pem(&data).expect(&format!(
                "Unable to construct a certificate from certs file set as `{}`",
                CERTS_ENV_VAR
            ));

            Some(cert)
        }
        _ => None,
    }
}
