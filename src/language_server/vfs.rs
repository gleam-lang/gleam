use std::collections::HashMap;
use std::io;
use std::io::ErrorKind;
use std::sync::RwLock;

use reqwest::blocking::Client;
use tower_lsp::lsp_types::Url;

#[derive(Debug)]
pub struct VFS {
    overlays: RwLock<HashMap<Url, String>>,

    http_client: Client,
}

impl VFS {
    pub fn new() -> io::Result<VFS> {
        let client = Client::builder()
            .gzip(true)
            .build()
            .map_err(categorise_reqwest_error)?;

        Ok(
            VFS {
                overlays: RwLock::new(HashMap::new()),
                http_client: client,
            }
        )
    }

    pub fn get_file_contents(&self, uri: &Url) -> io::Result<String> {
        {
            let hm = self.overlays.read().unwrap();

            if let Some(contents) = hm.get(uri) {
                return Ok(contents.clone());
            };
        }

        let contents = self.read_file_from_uri(uri)?;
        self.replace_file(uri, &contents);
        Ok(contents)
    }

    pub fn replace_file(&self, uri: &Url, contents: &str) {
        let mut hm = self.overlays.write().unwrap();
        hm.insert(uri.clone(), contents.to_string());
    }

    fn read_file_from_uri(&self, uri: &Url) -> io::Result<String> {
        let scheme = uri.scheme();
    
        if scheme == "file" {
            if uri.host_str().unwrap_or("localhost") != "localhost" {
                let contents = std::fs::read_to_string(uri.path())?;
    
                return Ok(contents);
            } else {
                return Err(io::Error::new(ErrorKind::AddrNotAvailable, "Remote host on file URI"));
            };
        };
        
        if scheme == "http" || scheme == "https" {
            let response = self.http_client.get(uri.as_str()).send().map_err(categorise_reqwest_error)?;
    
            match response.error_for_status() {
                Ok(res) => return res.text().map_err(categorise_reqwest_error),
                Err(error) => return Err(categorise_reqwest_error(error)),
            };
        };
        
        Err(io::Error::new(ErrorKind::AddrNotAvailable, format!("Unsupported URI scheme: {}", scheme))) 
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