use std::path::{Path, PathBuf};

use debug_ignore::DebugIgnore;

use crate::{
    io::{FileSystemIO, HttpClient},
    Error,
};

#[derive(Debug)]
pub struct Client {
    fs: DebugIgnore<Box<dyn FileSystemIO>>,
    http: DebugIgnore<Box<dyn HttpClient>>,
    hex_config: hexpm::Config,
    pub cache_directory: PathBuf,
}

impl Client {
    pub fn new(fs: Box<dyn FileSystemIO>, http: Box<dyn HttpClient>) -> Self {
        Self {
            fs: DebugIgnore(fs),
            http: DebugIgnore(http),
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
