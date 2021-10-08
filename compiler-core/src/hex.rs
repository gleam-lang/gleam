use debug_ignore::DebugIgnore;
use flate2::read::GzDecoder;
use futures::future;
use hexpm::version::{Manifest, ManifestPackage, Version};
use std::path::PathBuf;
use tar::Archive;

use crate::{
    config::PackageConfig,
    io::{FileSystemIO, HttpClient, TarUnpacker},
    paths, Error, Result,
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

#[derive(Debug)]
pub struct Downloader {
    fs: DebugIgnore<Box<dyn FileSystemIO>>,
    http: DebugIgnore<Box<dyn HttpClient>>,
    untar: DebugIgnore<Box<dyn TarUnpacker>>,
    hex_config: hexpm::Config,
}

impl Downloader {
    pub fn new(
        fs: Box<dyn FileSystemIO>,
        http: Box<dyn HttpClient>,
        untar: Box<dyn TarUnpacker>,
    ) -> Self {
        Self {
            fs: DebugIgnore(fs),
            http: DebugIgnore(http),
            untar: DebugIgnore(untar),
            hex_config: hexpm::Config::new(),
        }
    }

    async fn get_package_checksum(&self, name: &str, version: &Version) -> Result<Vec<u8>> {
        let config = hexpm::Config::new();
        let request = hexpm::get_package_request(name, None, &config);
        let response = self.http.send(request).await?;

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

    pub async fn ensure_package_downloaded(
        &self,
        package_name: &str,
        version: &Version,
    ) -> Result<bool, Error> {
        let checksum = self.get_package_checksum(package_name, version).await?;
        let tarball_path = paths::package_cache_tarball(package_name, &version.to_string());
        if self.fs.is_file(&tarball_path) {
            tracing::info!(
                package = package_name,
                version = %version,
                "Package already in cache"
            );
            return Ok(false);
        }
        tracing::info!(
            package = package_name,
            version = %version,
            "Downloading package to cache"
        );

        let request = hexpm::get_package_tarball_request(
            package_name,
            &version.to_string(),
            None,
            &self.hex_config,
        );
        let response = self.http.send(request).await?;

        let tarball =
            hexpm::get_package_tarball_response(response, &checksum).map_err(|error| {
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

    pub async fn ensure_package_in_target(&self, name: String, version: Version) -> Result<bool> {
        let _ = self.ensure_package_downloaded(&name, &version).await?;
        self.extract_package_from_cache(&name, &version)
    }

    // It would be really nice if this was async but the library is sync
    pub fn extract_package_from_cache(&self, name: &str, version: &Version) -> Result<bool> {
        let contents_path = PathBuf::from("contents.tar.gz");
        let destination = paths::build_deps_package(name);

        // If the directory already exists then there's nothing for us to do
        if self.fs.is_directory(&destination) {
            tracing::info!(package = name, "package already in target");
            return Ok(false);
        }

        tracing::info!(package = name, "writing package to target");
        let tarball = paths::package_cache_tarball(name, &version.to_string());
        let reader = self.fs.reader(&tarball)?;
        let mut archive = Archive::new(reader);

        // Find the source code from within the outer tarball
        for entry in self.untar.entries(&mut archive)? {
            let file = entry.map_err(Error::expand_tar)?;

            let path = file.header().path().map_err(Error::expand_tar)?;
            if path.as_ref() == contents_path {
                // Expand this inner source code and write to the file system
                let archive = Archive::new(GzDecoder::new(file));
                let result = self.untar.unpack(&destination, archive);

                // If we failed to expand the tarball remove any source code
                // that was partially written so that we don't mistakenly think
                // the operation succeeded next time we run.
                return match result {
                    Ok(()) => Ok(true),
                    Err(err) => {
                        self.fs.delete(&destination)?;
                        Err(err)
                    }
                };
            }
        }

        // TODO: return an error as it wasn't found
        unimplemented!()
    }

    pub async fn download_manifest_packages(
        &self,
        manifest: &Manifest,
        project_name: &str,
    ) -> Result<usize> {
        let futures = manifest
            .packages
            .iter()
            .flat_map(|package| match package {
                ManifestPackage::Hex { name, .. } if name == project_name => None,
                ManifestPackage::Hex { name, version } => Some((name.to_string(), version.clone())),
            })
            .map(|(name, version)| self.ensure_package_in_target(name, version));

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
}
