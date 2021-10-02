use futures::future;
use gleam_core::{hex, io::HttpClient as _, Error, Result};
use itertools::Itertools;

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

pub fn download(packages: Vec<String>) -> Result<()> {
    print_downloading("packages");

    let http = HttpClient::boxed();
    let fs = FileSystemAccessor::boxed();
    let downloader = hex::Downloader::new(fs, http);

    // Start event loop so we can run async functions to call the Hex API
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");

    // Prepare an async computation to download each package
    let futures = packages.into_iter().map(|package| async {
        let (name, version) = split_package_name_version(package)?;
        let checksum = get_package_checksum(&name, &version).await?;
        downloader
            .ensure_package_downloaded(&name, &version, &checksum)
            .await
    });

    // Run the futures to download the packages concurrently
    let results = runtime.block_on(future::join_all(futures));

    // Check for any errors
    results.into_iter().try_collect()?;

    Ok(())
}

fn split_package_name_version(package: String) -> Result<(String, String)> {
    let mut parts = package.split("@");
    if let (Some(name), Some(version), None) = (parts.next(), parts.next(), parts.next()) {
        Ok((name.to_string(), version.to_string()))
    } else {
        // Error!
        todo!("Bad package format: {}", package)
    }
}

async fn get_package_checksum(name: &str, version: &str) -> Result<Vec<u8>> {
    let config = hexpm::Config::new();
    let request = hexpm::get_package_request(name, None, &config);
    let response = HttpClient::new().send(request).await?;

    Ok(hexpm::get_package_response(response, HEXPM_PUBLIC_KEY)
        // TODO: Better error handling. Handle the package not existing.
        .map_err(Error::hex)?
        .releases
        .into_iter()
        .find(|p| p.version == version)
        .ok_or_else(|| {
            // TODO: version not found for package
            Error::Hex("Version not found".to_string())
        })?
        .outer_checksum)
}
