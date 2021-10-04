use std::time::Instant;

use gleam_core::{
    hex::{self, HEXPM_PUBLIC_KEY},
    io::{HttpClient as _, TarUnpacker},
    Result,
};

use crate::{
    cli::{print_downloading, print_packages_downloaded},
    fs::FileSystemAccessor,
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
    let manifest = hex::resolve_versions(PackageFetcher::boxed(runtime.handle().clone()), &config)?;

    // Download them from Hex to the local cache
    let count =
        runtime.block_on(downloader.download_manifest_packages(&manifest, &project_name))?;

    print_packages_downloaded(start, count);
    Ok(())
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
    fn io_result_unpack(
        &self,
        path: &std::path::Path,
        mut archive: tar::Archive<flate2::read::GzDecoder<gleam_core::io::WrappedReader>>,
    ) -> std::io::Result<()> {
        archive.unpack(path)
    }
}

impl hexpm::version::PackageFetcher for PackageFetcher {
    fn get_dependencies(
        &self,
        package: &str,
    ) -> Result<hexpm::Package, Box<dyn std::error::Error>> {
        let config = hexpm::Config::new();
        let request = hexpm::get_package_request(package, None, &config);
        let response = self
            .runtime
            .block_on(self.http.send(request))
            .map_err(Box::new)?;
        hexpm::get_package_response(response, HEXPM_PUBLIC_KEY).map_err(|e| e.into())
    }
}
