use std::convert::TryInto;

use async_trait::async_trait;
use bytes::Bytes;
use gleam_core::{Error, Result};
use http::{Request, Response};

#[derive(Debug)]
pub struct HttpClient(reqwest::Client);

#[async_trait]
impl gleam_core::io::HttpClient for HttpClient {
    async fn send(&self, request: Request<Vec<u8>>) -> Result<Response<Bytes>> {
        let request = request
            .try_into()
            .expect("Unable to convert HTTP request for use by reqwest library");
        let mut response = self.0.execute(request).await.map_err(Error::http)?;
        let mut builder = Response::builder()
            .status(response.status())
            .version(response.version());
        std::mem::swap(builder.headers_mut().unwrap(), response.headers_mut());
        builder
            .body(response.bytes().await.map_err(Error::http)?)
            .map_err(Error::http)
    }
}
