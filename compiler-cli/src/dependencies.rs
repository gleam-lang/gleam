use futures::future;
use gleam_core::{build::project_root::ProjectRoot, hex, io::HttpClient as _, Error, Result};
use hexpm::version::{ManifestPackage, Version};
use itertools::Itertools;
use std::path::PathBuf;

use crate::{cli::print_downloading, fs::FileSystemAccessor, http::HttpClient};

// TODO: move this elsewhere or pull from repo
// https://github.com/hexpm/specifications/blob/74dd7ef3956ee2bc7b8db9608e5ee909f5e08037/endpoints.md#endpoints-1
const HEXPM_PUBLIC_KEY: &[u8] = b"-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApqREcFDt5vV21JVe2QNB
Edvzk6w36aNFhVGWN5toNJRjRJ6m4hIuG4KaXtDWVLjnvct6MYMfqhC79HAGwyF+
IqR6Q6a5bbFSsImgBJwz1oadoVKD6ZNetAuCIK84cjMrEFRkELtEIPNHblCzUkkM
3rS9+DPlnfG8hBvGi6tvQIuZmXGCxF/73hU0/MyGhbmEjIKRtG6b0sJYKelRLTPW
XgK7s5pESgiwf2YC/2MGDXjAJfpfCd0RpLdvd4eRiXtVlE9qO9bND94E7PgQ/xqZ
J1i2xWFndWa6nfFnRxZmCStCOZWYYPlaxr+FZceFbpMwzTNs4g3d4tLNUcbKAIH4
0wIDAQAB
-----END PUBLIC KEY-----
";

pub fn download() -> Result<()> {
    print_downloading("packages");

    let http = HttpClient::boxed();
    let fs = FileSystemAccessor::boxed();
    let downloader = hex::Downloader::new(fs, http);

    // Read the project config
    let root = ProjectRoot::new(PathBuf::from("./"));
    let config = crate::config::root_config(&root)?;
    let project_name = config.name.clone();

    // Start event loop so we can run async functions to call the Hex API
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");

    let manifest = hexpm::version::resolve_versions(
        PackageFetcher::boxed(runtime.handle().clone()),
        config.name,
        config.version,
        config.dependencies.into_iter(),
    )
    // TODO: FIXME: error handling
    .expect("pubgrub error");

    // Prepare an async computation to download each package
    let futures = manifest.packages.into_iter().map(|package| async {
        match package {
            ManifestPackage::Hex { name, .. } if name == project_name => Ok(()),
            ManifestPackage::Hex { name, version } => {
                let checksum = get_package_checksum(&name, &version).await?;
                downloader
                    .ensure_package_downloaded(&name, &version, &checksum)
                    .await
            }
        }
    });

    // Run the futures to download the packages concurrently
    let results = runtime.block_on(future::join_all(futures));

    // Check for any errors
    results.into_iter().try_collect()?;

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

impl hexpm::version::PackageFetcher for PackageFetcher {
    fn get_dependencies(&self, package: &str) -> Result<hexpm::Package, hexpm::ApiError> {
        let config = hexpm::Config::new();
        let request = hexpm::get_package_request(package, None, &config);
        //
        //
        //
        //
        // TODO: FIXME: Make the hexpm library more flexible with errors here.
        // We need to wire in out error type, but the library is limited to its
        // own.
        //
        //
        //
        let response = self
            .runtime
            .block_on(self.http.send(request))
            .expect("TODO! FIX ME");
        hexpm::get_package_response(response, HEXPM_PUBLIC_KEY)
    }
}

async fn get_package_checksum(name: &str, version: &Version) -> Result<Vec<u8>> {
    let config = hexpm::Config::new();
    let request = hexpm::get_package_request(name, None, &config);
    let response = HttpClient::new().send(request).await?;

    Ok(hexpm::get_package_response(response, HEXPM_PUBLIC_KEY)
        // TODO: Better error handling. Handle the package not existing.
        .map_err(Error::hex)?
        .releases
        .into_iter()
        .find(|p| &p.version == version)
        .ok_or_else(|| {
            // TODO: version not found for package
            Error::Hex("Version not found".to_string())
        })?
        .outer_checksum)
}
