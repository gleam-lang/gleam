use std::{collections::HashMap, time::Instant};

use flate2::read::GzDecoder;
use gleam_core::{
    build::Mode,
    config::PackageConfig,
    error::{FileIoAction, FileKind},
    hex::{self, HEXPM_PUBLIC_KEY},
    io::{HttpClient as _, TarUnpacker, WrappedReader},
    paths, Error, Result,
};
use hexpm::version::Version;

use crate::{
    cli::{print_downloading, print_packages_downloaded},
    fs::{self, FileSystemAccessor},
    http::HttpClient,
};

pub fn download() -> Result<()> {
    print_downloading("packages");
    let start = Instant::now();
    let mode = Mode::Dev;

    let http = HttpClient::boxed();
    let fs = FileSystemAccessor::boxed();
    let downloader = hex::Downloader::new(fs, http, Untar::boxed());

    // Read the project config
    let config = crate::config::root_config()?;
    let project_name = config.name.clone();

    // Start event loop so we can run async functions to call the Hex API
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");

    // Determine what versions we need
    let manifest = get_manifest(runtime.handle().clone(), mode, &config)?;

    // Remove any packages that are no longer required due to gleam.toml changes
    remove_extra_packages(&manifest)?;

    // Download them from Hex to the local cache
    tracing::info!("Downloading packages");
    let count =
        runtime.block_on(downloader.download_hex_packages(&manifest.packages, &project_name))?;

    // Record new state of the packages directory
    LocalPackages::from_manifest(manifest).write()?;

    // TODO: we should print the number of deps new to ./target, not to the shared cache
    print_packages_downloaded(start, count);
    Ok(())
}

fn gleam_toml_md5() -> Result<String> {
    let checksum = &md5::compute(&fs::read(paths::root_config())?).to_vec();
    Ok(base64::encode(checksum))
}

fn remove_extra_packages(manifest: &Manifest) -> Result<()> {
    let extra = match LocalPackages::read()? {
        Some(extra) => extra,
        None => return Ok(()),
    };
    for (package, version) in extra.extra_local_packages(manifest) {
        let path = paths::build_deps_package(&package);
        if path.exists() {
            tracing::info!(package=%package, version=%version, "removing_unneeded_package");
            fs::delete_dir(&path)?;
        }
    }
    Ok(())
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct Manifest {
    config_checksum: String,
    packages: HashMap<String, Version>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct LocalPackages {
    packages: HashMap<String, Version>,
}

impl LocalPackages {
    // TODO: test
    pub fn extra_local_packages(&self, manifest: &Manifest) -> Vec<(String, String)> {
        let mut extra = Vec::new();
        for (name, version) in &self.packages {
            if manifest.packages.get(name.as_str()) != Some(version) {
                extra.push((name.to_string(), version.to_string()));
            }
        }
        extra
    }

    pub fn read() -> Result<Option<Self>> {
        let path = paths::packages_toml();
        if !path.exists() {
            return Ok(None);
        }
        let toml = crate::fs::read(&path)?;
        Ok(Some(toml::from_str(&toml).map_err(|e| Error::FileIo {
            action: FileIoAction::Parse,
            kind: FileKind::File,
            path: path.clone(),
            err: Some(e.to_string()),
        })?))
    }

    pub fn write(&self) -> Result<()> {
        let path = paths::packages_toml();
        let toml = toml::to_string(&self).expect("packages.toml serialization");
        fs::write(&path, &toml)
    }

    pub fn from_manifest(manifest: Manifest) -> Self {
        Self {
            packages: manifest.packages,
        }
    }
}

#[test]
fn extra_local_packages() {
    let mut extra = LocalPackages {
        packages: vec![
            ("local1".to_string(), Version::parse("1.0.0").unwrap()),
            ("local2".to_string(), Version::parse("2.0.0").unwrap()),
            ("local3".to_string(), Version::parse("3.0.0").unwrap()),
        ]
        .into_iter()
        .collect(),
    }
    .extra_local_packages(&Manifest {
        config_checksum: "".to_string(),
        packages: vec![
            ("local1".to_string(), Version::parse("1.0.0").unwrap()),
            ("local2".to_string(), Version::parse("3.0.0").unwrap()),
        ]
        .into_iter()
        .collect(),
    });
    extra.sort();
    assert_eq!(
        extra,
        vec![
            ("local2".to_string(), "2.0.0".to_string()),
            ("local3".to_string(), "3.0.0".to_string()),
        ]
    )
}

fn get_manifest(
    runtime: tokio::runtime::Handle,

    mode: Mode,
    config: &PackageConfig,
) -> Result<Manifest> {
    let manifest_path = paths::manifest_path();
    if manifest_path.exists() {
        // If the manifest exists we read it and use that the versions specified
        // in there
        tracing::info!("Reading manifest.toml");
        let toml = crate::fs::read(&manifest_path)?;
        toml::from_str(&toml).map_err(|e| Error::FileIo {
            action: FileIoAction::Parse,
            kind: FileKind::File,
            path: manifest_path.clone(),
            err: Some(e.to_string()),
        })
    } else {
        // If there is no manifest then we resolve the versions from their
        // specified requirements in the Hex API
        tracing::info!("Resolving Hex package versions");
        let manifest = Manifest {
            packages: hex::resolve_versions(PackageFetcher::boxed(runtime), mode, config)?,
            config_checksum: gleam_toml_md5()?,
        };
        let toml = toml::to_string(&manifest).expect("manifest.toml serialization");
        tracing::info!("Writing manifest.toml");
        fs::write(&manifest_path, &toml)?;
        Ok(manifest)
    }
}

struct PackageFetcher {
    runtime: tokio::runtime::Handle,
    http: HttpClient,
}

impl PackageFetcher {
    pub fn boxed(runtime: tokio::runtime::Handle) -> Box<Self> {
        Box::new(Self {
            runtime,
            http: HttpClient::new(),
        })
    }
}

#[derive(Debug)]
pub struct Untar;

impl Untar {
    pub fn boxed() -> Box<Self> {
        Box::new(Self)
    }
}

impl TarUnpacker for Untar {
    fn io_result_entries<'a>(
        &self,
        archive: &'a mut tar::Archive<WrappedReader>,
    ) -> std::io::Result<tar::Entries<'a, WrappedReader>> {
        archive.entries()
    }

    fn io_result_unpack(
        &self,
        path: &std::path::Path,
        mut archive: tar::Archive<GzDecoder<tar::Entry<'_, WrappedReader>>>,
    ) -> std::io::Result<()> {
        archive.unpack(path)
    }
}

impl hexpm::version::PackageFetcher for PackageFetcher {
    fn get_dependencies(
        &self,
        package: &str,
    ) -> Result<hexpm::Package, Box<dyn std::error::Error>> {
        tracing::info!(package = package, "Looking up package in Hex API");
        let config = hexpm::Config::new();
        let request = hexpm::get_package_request(package, None, &config);
        let response = self
            .runtime
            .block_on(self.http.send(request))
            .map_err(Box::new)?;
        hexpm::get_package_response(response, HEXPM_PUBLIC_KEY).map_err(|e| e.into())
    }
}
