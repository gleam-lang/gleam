use crate::{Error, Result};

use debug_ignore::DebugIgnore;
use futures::future;
use hexpm::version::{Manifest, ManifestPackage, Version};

use crate::{
    config::PackageConfig,
    io::{FileSystemIO, HttpClient},
    paths,
};

pub const HEXPM_PUBLIC_KEY: &[u8] = b"-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApqREcFDt5vV21JVe2QNB
Edvzk6w36aNFhVGWN5toNJRjRJ6m4hIuG4KaXtDWVLjnvct6MYMfqhC79HAGwyF+
IqR6Q6a5bbFSsImgBJwz1oadoVKD6ZNetAuCIK84cjMrEFRkELtEIPNHblCzUkkM
3rS9+DPlnfG8hBvGi6tvQIuZmXGCxF/73hU0/MyGhbmEjIKRtG6b0sJYKelRLTPW
XgK7s5pESgiwf2YC/2MGDXjAJfpfCd0RpLdvd4eRiXtVlE9qO9bND94E7PgQ/xqZ
J1i2xWFndWa6nfFnRxZmCStCOZWYYPlaxr+FZceFbpMwzTNs4g3d4tLNUcbKAIH4
0wIDAQAB
-----END PUBLIC KEY-----
";

pub fn resolve_versions(
    package_fetcher: Box<dyn hexpm::version::PackageFetcher>,
    config: &PackageConfig,
) -> Result<Manifest> {
    let specified_dependencies = config
        .dependencies
        .iter()
        .map(|(a, b)| (a.clone(), b.clone()));
    hexpm::version::resolve_versions(
        package_fetcher,
        config.name.clone(),
        config.version.clone(),
        specified_dependencies,
    )
    .map_err(Error::dependency_resolution_failed)
}

async fn get_package_checksum<Http: HttpClient>(
    http: &Http,
    name: &str,
    version: &Version,
) -> Result<Vec<u8>> {
    let config = hexpm::Config::new();
    let request = hexpm::get_package_request(name, None, &config);
    let response = http.send(request).await?;

    Ok(hexpm::get_package_response(response, HEXPM_PUBLIC_KEY)
        .map_err(Error::hex)?
        .releases
        .into_iter()
        .find(|p| &p.version == version)
        .ok_or_else(|| {
            Error::Hex(format!(
                "package {}@{} not found",
                name,
                version.to_string()
            ))
        })?
        .outer_checksum)
}

// pub async fn extract_package_from_cache<FileSystem: FileSystemIO>(
//     name: &str,
//     version: &Version,
//     fs: FileSystem,
// ) -> Result<()> {
//     let tarball = paths::package_cache_tarball(name, &version.to_string());
//     // let reader = fs.reader(&tarball)?;
//     // let mut archive = Archive::new(GzDecoder::new(reader));
//     // archive.unpack(".")?;
//     Ok(())
// }

pub async fn download_package_to_cache<Http: HttpClient>(
    name: String,
    version: Version,
    downloader: &Downloader,
    http: &Http,
) -> Result<bool> {
    let checksum = get_package_checksum(http, &name, &version).await?;
    downloader
        .ensure_package_downloaded(&name, &version, &checksum)
        .await
}

pub async fn download_manifest_packages_to_cache<Http: HttpClient>(
    manifest: &Manifest,
    downloader: &Downloader,
    http: &Http,
    project_name: &str,
) -> Result<usize> {
    let futures = manifest
        .packages
        .iter()
        .flat_map(|package| match package {
            ManifestPackage::Hex { name, .. } if name == &project_name => None,
            ManifestPackage::Hex { name, version } => Some((name.to_string(), version.clone())),
        })
        .map(|(name, version)| download_package_to_cache(name, version, downloader, http));

    // Run the futures to download the packages concurrently
    let results = future::join_all(futures).await;

    // Count the number of packages downloaded while checking for errors
    let mut count = 0;
    for result in results {
        if result? {
            count += 1;
        }
    }
    Ok(count)
}

#[derive(Debug)]
pub struct Downloader {
    fs: DebugIgnore<Box<dyn FileSystemIO>>,
    http: DebugIgnore<Box<dyn HttpClient>>,
    hex_config: hexpm::Config,
}

impl Downloader {
    pub fn new(fs: Box<dyn FileSystemIO>, http: Box<dyn HttpClient>) -> Self {
        Self {
            fs: DebugIgnore(fs),
            http: DebugIgnore(http),
            hex_config: hexpm::Config::new(),
        }
    }

    pub async fn ensure_package_downloaded(
        &self,
        package_name: &str,
        version: &Version,
        checksum: &[u8],
    ) -> Result<bool, Error> {
        let tarball_path = paths::package_cache_tarball(package_name, &version.to_string());
        if self.fs.is_file(&tarball_path) {
            tracing::info!(
                package = package_name,
                version = %version,
                "Package already downloaded"
            );
            return Ok(false);
        }
        tracing::info!(
            package = package_name,
            version = %version,
            "Downloading package"
        );

        let request = hexpm::get_package_tarball_request(
            package_name,
            &version.to_string(),
            None,
            &self.hex_config,
        );
        let response = self.http.send(request).await?;

        let tarball = hexpm::get_package_tarball_response(response, checksum).map_err(|error| {
            Error::DownloadPackageError {
                package_name: package_name.to_string(),
                package_version: version.to_string(),
                error: error.to_string(),
            }
        })?;
        let mut file = self.fs.writer(&tarball_path)?;
        file.write(&tarball)?;
        Ok(true)
    }
}
