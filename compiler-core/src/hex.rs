use camino::Utf8Path;
use debug_ignore::DebugIgnore;
use ecow::EcoString;
use flate2::read::GzDecoder;
use futures::future;
use hexpm::{ApiError, version::Version};
use tar::Archive;

use crate::{
    Error, Result,
    io::{FileSystemReader, FileSystemWriter, HttpClient, TarUnpacker},
    manifest::{ManifestPackage, ManifestPackageSource},
    paths::{self, ProjectPaths},
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

fn key_name(hostname: &str) -> String {
    format!("gleam-{hostname}")
}

pub async fn publish_package<Http: HttpClient>(
    release_tarball: Vec<u8>,
    version: String,
    name: &str,
    api_key: &str,
    config: &hexpm::Config,
    replace: bool,
    http: &Http,
) -> Result<()> {
    tracing::info!("Publishing package, replace: {}", replace);
    let request = hexpm::api_publish_package_request(release_tarball, api_key, config, replace);
    let response = http.send(request).await?;
    hexpm::api_publish_package_response(response).map_err(|e| match e {
        ApiError::NotReplacing => Error::HexPublishReplaceRequired { version },
        ApiError::Forbidden => Error::HexPackageAlreadyExists {
            name: name.into(),
            version,
        },
        _ => Error::hex(e),
    })
}

pub async fn transfer_owner<Http: HttpClient>(
    api_key: &str,
    package_name: String,
    new_owner_username_or_email: String,
    config: &hexpm::Config,
    http: &Http,
) -> Result<()> {
    tracing::info!(
        "Transferring ownership of `{}` to {}",
        package_name,
        new_owner_username_or_email
    );
    let request = hexpm::api_transfer_owner_request(
        &package_name,
        &new_owner_username_or_email,
        api_key,
        config,
    );
    let response = http.send(request).await?;
    hexpm::api_transfer_owner_response(response).map_err(Error::hex)
}

#[derive(Debug, strum::EnumString, strum::VariantNames, Clone, Copy, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum RetirementReason {
    Other,
    Invalid,
    Security,
    Deprecated,
    Renamed,
}

impl RetirementReason {
    pub fn to_library_enum(&self) -> hexpm::RetirementReason {
        match self {
            RetirementReason::Other => hexpm::RetirementReason::Other,
            RetirementReason::Invalid => hexpm::RetirementReason::Invalid,
            RetirementReason::Security => hexpm::RetirementReason::Security,
            RetirementReason::Deprecated => hexpm::RetirementReason::Deprecated,
            RetirementReason::Renamed => hexpm::RetirementReason::Renamed,
        }
    }
}

pub async fn retire_release<Http: HttpClient>(
    package: &str,
    version: &str,
    reason: RetirementReason,
    message: Option<&str>,
    api_key: &str,
    config: &hexpm::Config,
    http: &Http,
) -> Result<()> {
    tracing::info!(package=%package, version=%version, "retiring_hex_release");
    let request = hexpm::api_retire_release_request(
        package,
        version,
        reason.to_library_enum(),
        message,
        api_key,
        config,
    );
    let response = http.send(request).await?;
    hexpm::api_retire_release_response(response).map_err(Error::hex)
}

pub async fn unretire_release<Http: HttpClient>(
    package: &str,
    version: &str,
    api_key: &str,
    config: &hexpm::Config,
    http: &Http,
) -> Result<()> {
    tracing::info!(package=%package, version=%version, "retiring_hex_release");
    let request = hexpm::api_unretire_release_request(package, version, api_key, config);
    let response = http.send(request).await?;
    hexpm::api_unretire_release_response(response).map_err(Error::hex)
}

pub async fn create_api_key<Http: HttpClient>(
    hostname: &str,
    username: &str,
    password: &str,
    config: &hexpm::Config,
    http: &Http,
) -> Result<String> {
    tracing::info!("Creating API key with Hex");
    let request =
        hexpm::api_create_api_key_request(username, password, &key_name(hostname), config);
    let response = http.send(request).await?;
    hexpm::api_create_api_key_response(response).map_err(Error::hex)
}

pub async fn remove_api_key<Http: HttpClient>(
    hostname: &str,
    config: &hexpm::Config,
    auth_key: &str,
    http: &Http,
) -> Result<()> {
    tracing::info!("Deleting API key from Hex");
    let request = hexpm::api_remove_api_key_request(&key_name(hostname), auth_key, config);
    let response = http.send(request).await?;
    hexpm::api_remove_api_key_response(response).map_err(Error::hex)
}

#[derive(Debug)]
pub struct Downloader {
    fs_reader: DebugIgnore<Box<dyn FileSystemReader>>,
    fs_writer: DebugIgnore<Box<dyn FileSystemWriter>>,
    http: DebugIgnore<Box<dyn HttpClient>>,
    untar: DebugIgnore<Box<dyn TarUnpacker>>,
    hex_config: hexpm::Config,
    paths: ProjectPaths,
}

impl Downloader {
    pub fn new(
        fs_reader: Box<dyn FileSystemReader>,
        fs_writer: Box<dyn FileSystemWriter>,
        http: Box<dyn HttpClient>,
        untar: Box<dyn TarUnpacker>,
        paths: ProjectPaths,
    ) -> Self {
        Self {
            fs_reader: DebugIgnore(fs_reader),
            fs_writer: DebugIgnore(fs_writer),
            http: DebugIgnore(http),
            untar: DebugIgnore(untar),
            hex_config: hexpm::Config::new(),
            paths,
        }
    }

    pub async fn ensure_package_downloaded(
        &self,
        package: &ManifestPackage,
    ) -> Result<bool, Error> {
        let outer_checksum = match &package.source {
            ManifestPackageSource::Hex { outer_checksum } => outer_checksum,
            ManifestPackageSource::Git { .. } | ManifestPackageSource::Local { .. } => {
                panic!("Attempt to download non-hex package from hex")
            }
        };

        let tarball_path = paths::global_package_cache_package_tarball(
            &package.name,
            &package.version.to_string(),
        );
        if self.fs_reader.is_file(&tarball_path) {
            tracing::info!(
                package = package.name.as_str(),
                version = %package.version,
                "package_in_cache"
            );
            return Ok(false);
        }
        tracing::info!(
            package = &package.name.as_str(),
            version = %package.version,
            "downloading_package_to_cache"
        );

        let request = hexpm::repository_get_package_tarball_request(
            &package.name,
            &package.version.to_string(),
            None,
            &self.hex_config,
        );
        let response = self.http.send(request).await?;

        let tarball = hexpm::repository_get_package_tarball_response(response, &outer_checksum.0)
            .map_err(|error| Error::DownloadPackageError {
            package_name: package.name.to_string(),
            package_version: package.version.to_string(),
            error: error.to_string(),
        })?;
        self.fs_writer.write_bytes(&tarball_path, &tarball)?;
        Ok(true)
    }

    pub async fn ensure_package_in_build_directory(
        &self,
        package: &ManifestPackage,
    ) -> Result<bool> {
        let _ = self.ensure_package_downloaded(package).await?;
        self.extract_package_from_cache(&package.name, &package.version)
    }

    // It would be really nice if this was async but the library is sync
    pub fn extract_package_from_cache(&self, name: &str, version: &Version) -> Result<bool> {
        let contents_path = Utf8Path::new("contents.tar.gz");
        let destination = self.paths.build_packages_package(name);

        // If the directory already exists then there's nothing for us to do
        if self.fs_reader.is_directory(&destination) {
            tracing::info!(package = name, "Package already in build directory");
            return Ok(false);
        }

        tracing::info!(package = name, "writing_package_to_target");
        let tarball = paths::global_package_cache_package_tarball(name, &version.to_string());
        let reader = self.fs_reader.reader(&tarball)?;
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
                        self.fs_writer.delete_directory(&destination)?;
                        Err(err)
                    }
                };
            }
        }

        Err(Error::ExpandTar {
            error: "Unable to locate Hex package contents.tar.gz".into(),
        })
    }

    pub async fn download_hex_packages<'a, Packages: Iterator<Item = &'a ManifestPackage>>(
        &self,
        packages: Packages,
        project_name: &str,
    ) -> Result<()> {
        let futures = packages
            .filter(|package| project_name != package.name)
            .map(|package| self.ensure_package_in_build_directory(package));

        // Run the futures to download the packages concurrently
        let results = future::join_all(futures).await;

        // Count the number of packages downloaded while checking for errors
        for result in results {
            let _ = result?;
        }
        Ok(())
    }
}

pub async fn publish_documentation<Http: HttpClient>(
    name: &str,
    version: &Version,
    archive: Vec<u8>,
    api_key: &str,
    config: &hexpm::Config,
    http: &Http,
) -> Result<()> {
    tracing::info!("publishing_documentation");
    let request =
        hexpm::api_publish_docs_request(name, &version.to_string(), archive, api_key, config)
            .map_err(Error::hex)?;
    let response = http.send(request).await?;
    hexpm::api_publish_docs_response(response).map_err(Error::hex)
}

pub async fn get_package_release<Http: HttpClient>(
    name: &str,
    version: &Version,
    config: &hexpm::Config,
    http: &Http,
) -> Result<hexpm::Release<hexpm::ReleaseMeta>> {
    let version = version.to_string();
    tracing::info!(
        name = name,
        version = version.as_str(),
        "looking_up_package_release"
    );
    let request = hexpm::api_get_package_release_request(name, &version, None, config);
    let response = http.send(request).await?;
    hexpm::api_get_package_release_response(response).map_err(Error::hex)
}
