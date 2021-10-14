use std::{path::PathBuf, time::Instant};

use flate2::read::GzDecoder;
use gleam_core::{
    config::PackageConfig,
    error::{FileIoAction, FileKind},
    hex::{self, HEXPM_PUBLIC_KEY},
    io::{HttpClient as _, TarUnpacker, WrappedReader},
    Error, Result,
};
use hexpm::version::Manifest;

use crate::{
    cli::{print_downloading, print_packages_downloaded},
    fs::{self, FileSystemAccessor},
    http::HttpClient,
};

pub fn download() -> Result<()> {
    print_downloading("packages");
    let start = Instant::now();

    let http = HttpClient::boxed();
    let fs = FileSystemAccessor::boxed();
    let downloader = hex::Downloader::new(fs, http, Untar::boxed());

    // Read the project config
    let config = crate::config::root_config()?;
    let project_name = config.name.clone();

    // Start event loop so we can run async functions to call the Hex API
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");

    // Determine what versions we need
    let manifest = get_manifest(runtime.handle().clone(), &config)?;

    // Download them from Hex to the local cache
    tracing::info!("Downloading packages");
    let count =
        runtime.block_on(downloader.download_manifest_packages(&manifest, &project_name))?;

    // TODO: we should print the number of deps new to ./target, not to the shared cache
    print_packages_downloaded(start, count);
    Ok(())
}

const MANIFEST_PATH: &str = "manifest.toml";

pub fn get_manifest(runtime: tokio::runtime::Handle, config: &PackageConfig) -> Result<Manifest> {
    let manifest_path = PathBuf::from(MANIFEST_PATH);
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
        let manifest = hex::resolve_versions(PackageFetcher::boxed(runtime), &config)?;
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
