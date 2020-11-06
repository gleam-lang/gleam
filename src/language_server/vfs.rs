use std::collections::HashMap;
use std::io;
use std::io::ErrorKind;
use std::sync::RwLock;

use super::document::Document;

use reqwest::blocking::Client;
use tower_lsp::lsp_types::Url;

#[derive(Debug)]
pub struct VFS {
    overlays: RwLock<HashMap<Url, RwLock<Document>>>,

    http_client: Client,
}

impl VFS {
    pub fn new() -> io::Result<VFS> {
        let client = Client::builder()
            .gzip(true)
            .build()
            .map_err(categorise_reqwest_error)?;

        Ok(VFS {
            overlays: RwLock::new(HashMap::new()),
            http_client: client,
        })
    }

    pub fn get_document_contents(&self, uri: &Url) -> io::Result<String> {
        {
            let hm = self.overlays.read().unwrap();

            if let Some(doc_lock) = hm.get(uri) {
                let document = doc_lock.read().unwrap();
                return Ok(document.contents().to_string());
            };
        }

        let contents = self.read_file_from_uri(uri)?;
        self.create_document(uri, &contents);
        Ok(contents)
    }

    pub fn modify_document<F>(&self, uri: &Url, f: F) -> Option<()>
    where
        F: Fn(&mut Document),
    {
        let hm = self.overlays.read().unwrap();
        match hm.get(uri) {
            Some(doc_lock) => {
                let document = &mut *doc_lock.write().unwrap();

                f(document);
                Some(())
            }
            None => None,
        }
    }

    #[allow(dead_code)]
    pub fn with_document<F, R>(&self, uri: &Url, f: F) -> Option<R>
    where
        F: Fn(&Document) -> R,
    {
        let hm = self.overlays.read().unwrap();

        match hm.get(uri) {
            Some(doc_lock) => {
                let document = &*doc_lock.read().unwrap();

                Some(f(document))
            }
            None => None,
        }
    }

    pub fn create_document(&self, uri: &Url, contents: &str) {
        let mut hm = self.overlays.write().unwrap();

        let doc = Document::new(contents.to_string());
        let doc_lock = RwLock::new(doc);

        hm.insert(uri.clone(), doc_lock);
    }

    pub fn evict_document(&self, uri: &Url) {
        let mut hm = self.overlays.write().unwrap();

        hm.remove(uri);
    }

    fn read_file_from_uri(&self, uri: &Url) -> io::Result<String> {
        let scheme = uri.scheme();
        if scheme == "file" {
            if uri.host_str().unwrap_or("localhost") != "localhost" {
                let contents = std::fs::read_to_string(uri.path())?;
                return Ok(contents);
            } else {
                return Err(io::Error::new(
                    ErrorKind::AddrNotAvailable,
                    "Remote host on file URI",
                ));
            };
        };
        if scheme == "http" || scheme == "https" {
            let response = self
                .http_client
                .get(uri.as_str())
                .send()
                .map_err(categorise_reqwest_error)?;
            match response.error_for_status() {
                Ok(res) => return res.text().map_err(categorise_reqwest_error),
                Err(error) => return Err(categorise_reqwest_error(error)),
            };
        };

        Err(io::Error::new(
            ErrorKind::AddrNotAvailable,
            format!("Unsupported URI scheme: {}", scheme),
        ))
    }
}

fn categorise_reqwest_error(error: reqwest::Error) -> io::Error {
    if error.is_timeout() {
        io::Error::new(ErrorKind::TimedOut, error)
    } else if error.is_body() || error.is_decode() {
        io::Error::new(ErrorKind::InvalidData, error)
    } else if let Some(status) = error.status().map(|s| s.as_u16()) {
        if status == 404 {
            io::Error::new(ErrorKind::NotFound, error)
        } else if status >= 401 && status <= 403 {
            io::Error::new(ErrorKind::PermissionDenied, error)
        } else {
            io::Error::new(ErrorKind::Other, error)
        }
    } else {
        io::Error::new(ErrorKind::Other, error)
    }
}

#[cfg(test)]
mod test {
    #![allow(warnings)]

    use super::*;

    use std::error::Error;

    type TestResult = Result<(), Box<dyn Error>>;
}
