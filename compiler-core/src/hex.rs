use std::path::{Path, PathBuf};

use crate::{
    io::{FileSystemIO, HttpClient},
    Error,
};

// TODO: abstract away the IO portion of this module so that a HTTP client or
// Hex client is injected in, or something like that...
// The current design of the hex client is... not amazing. It tries to be clever
// but you can't build a trait object with it, making it rather awkward.

pub struct Client {
    fs: Box<dyn FileSystemIO>,
    http: Box<dyn HttpClient>,
    hex_config: hexpm::Config,
    pub cache_directory: PathBuf,
}

impl std::fmt::Debug for Client {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Client")
            .field("fs", &"<FileSystemWriter>")
            .finish()
    }
}

impl Client {
    pub fn new(fs: Box<dyn FileSystemIO>, http: Box<dyn HttpClient>) -> Self {
        Self {
            fs,
            http,
            hex_config: hexpm::Config::new(),
            cache_directory: default_gleam_cache_directory(),
        }
    }

    pub async fn ensure_package_downloaded(
        &self,
        package_name: &str,
        version: &str,
        checksum: &[u8],
    ) -> Result<(), Error> {
        let tarball_path = package_cache_tarball_path(&self.cache_directory, package_name, version);
        if self.fs.is_file(&tarball_path) {
            tracing::info!(
                package = package_name,
                version = version,
                "Package already downloaded"
            );
            return Ok(());
        }
        tracing::info!(
            package = package_name,
            version = version,
            "Downloading package"
        );

        let request =
            hexpm::get_package_tarball_request(package_name, version, None, &self.hex_config);
        let response = self.http.send(request.map(String::into_bytes)).await?;

        let tarball = hexpm::get_package_tarball_response(response, checksum).map_err(|error| {
            Error::DownloadPackageError {
                package_name: package_name.to_string(),
                package_version: version.to_string(),
                error: error.to_string(),
            }
        })?;
        let mut file = self.fs.open(&tarball_path)?;
        file.write(&tarball)?;
        Ok(())
    }
}

fn package_cache_tarball_path(cache_path: &Path, package_name: &str, version: &str) -> PathBuf {
    packages_cache_directory(cache_path).join(format!("{}-{}.tar", package_name, version))
}

fn packages_cache_directory(cache_path: &Path) -> PathBuf {
    cache_path.join("hex").join("hexpm").join("packages")
}

fn default_gleam_cache_directory() -> PathBuf {
    dirs::cache_dir()
        .expect("Failed to determine user cache directory")
        .join("gleam")
}

#[test]
fn paths() {
    assert!(default_gleam_cache_directory().ends_with("gleam"));

    assert_eq!(
        packages_cache_directory(&PathBuf::from("/some/where")),
        PathBuf::from("/some/where/hex/hexpm/packages")
    );

    assert_eq!(
        package_cache_tarball_path(&PathBuf::from("/some/where"), "gleam_stdlib", "0.17.1"),
        PathBuf::from("/some/where/hex/hexpm/packages/gleam_stdlib-0.17.1.tar")
    );

    assert_eq!(
        package_cache_tarball_path(&PathBuf::from("/some/where"), "elli", "1.0.0"),
        PathBuf::from("/some/where/hex/hexpm/packages/elli-1.0.0.tar")
    );
}
