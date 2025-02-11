use std::convert::TryInto;
use std::sync::OnceLock;

use async_trait::async_trait;
use camino::Utf8PathBuf;
use gleam_core::{
    error::{FileIoAction, FileKind},
    Error, Result,
};
use http::{Request, Response};
use reqwest::{Certificate, Client};

use crate::fs;

static REQWEST_CLIENT: OnceLock<Client> = OnceLock::new();

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
        let client = init_client().map_err(Error::http)?;
        let mut response = client.execute(request).await.map_err(Error::http)?;
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

fn init_client() -> Result<&'static Client, Error> {
    if let Some(client) = REQWEST_CLIENT.get() {
        return Ok(client);
    }

    let certificate_path = match std::env::var("GLEAM_CACERTS_PATH") {
        Ok(path) => path,
        Err(_) => {
            return Ok(REQWEST_CLIENT.get_or_init(|| {
                Client::builder()
                    .build()
                    .expect("Failed to create reqwest client")
            }));
        }
    };

    let certificate_bytes = fs::read_bytes(&certificate_path)?;
    let certificate = Certificate::from_pem(&certificate_bytes).map_err(|error| Error::FileIo {
        kind: FileKind::File,
        action: FileIoAction::Parse,
        path: Utf8PathBuf::from(&certificate_path),
        err: Some(error.to_string()),
    })?;

    Ok(REQWEST_CLIENT.get_or_init(|| {
        Client::builder()
            .add_root_certificate(certificate)
            .build()
            .expect("Failed to create reqwest client")
    }))
}
