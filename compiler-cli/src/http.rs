// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

use std::convert::TryInto;
use std::sync::OnceLock;

use async_trait::async_trait;
use camino::Utf8PathBuf;
use gleam_core::{
    Error, Result,
    error::{FileIoAction, FileKind},
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
        tracing::debug!(
            method = request.method().as_str(),
            url = request.uri().to_string(),
            "http-send",
        );
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

    let client = build_client()?;
    Ok(REQWEST_CLIENT.get_or_init(|| client))
}

/// Build the HTTP client with an appropriate certificate trust store.
///
/// On most platforms reqwest's default `rustls` configuration uses
/// `rustls-platform-verifier`, which delegates to the operating system's trust
/// manager. This respects OS-installed and enterprise root certificates along
/// with the system's own trust decisions.
///
/// On Android that verifier reaches the trust manager by calling into the JVM,
/// and without it panics on the first request (see issue #5823). There we read
/// the same system trust store from the filesystem and configure those roots
/// explicitly instead.
fn build_client() -> Result<Client, Error> {
    if cfg!(target_os = "android") {
        let mut certificates = system_certificates();
        if let Ok(certificate_path) = std::env::var("GLEAM_CACERTS_PATH") {
            let certificate = read_certificate(&certificate_path)?;
            certificates.push(certificate);
        }

        Client::builder()
            .tls_certs_only(certificates)
            .build()
            .map_err(Error::http)
    } else {
        let Ok(certificate_path) = std::env::var("GLEAM_CACERTS_PATH") else {
            return Client::builder().build().map_err(Error::http);
        };

        tracing::trace!("Using GLEAM_CACERTS_PATH environment variable");
        let certificate = read_certificate(&certificate_path)?;
        Client::builder()
            .add_root_certificate(certificate)
            .build()
            .map_err(Error::http)
    }
}

fn read_certificate(path: &str) -> Result<Certificate, Error> {
    let bytes = fs::read_bytes(path)?;
    Certificate::from_pem(&bytes).map_err(|error| Error::FileIo {
        kind: FileKind::File,
        action: FileIoAction::Parse,
        path: Utf8PathBuf::from(path),
        err: Some(error.to_string()),
    })
}

/// Load the system trust store (only used on Android)
fn system_certificates() -> Vec<Certificate> {
    let loaded = rustls_native_certs::load_native_certs();
    for error in &loaded.errors {
        tracing::warn!("Failed to load a system certificate: {error}");
    }

    loaded
        .certs
        .iter()
        .filter_map(|certificate| Certificate::from_der(certificate.as_ref()).ok())
        .collect()
}
