use std::{
    io::Write,
    path::{Path, PathBuf},
};

use flate2::{write::GzEncoder, Compression};
use gleam_core::{hex::HEXPM_PUBLIC_KEY, io::HttpClient as _, Error, Result};

use crate::{build, cli, fs, http::HttpClient};

pub fn command() -> Result<()> {
    tokio::runtime::Runtime::new()
        .expect("Unable to start Tokio async runtime")
        .block_on(perform_command())
}

pub async fn perform_command() -> Result<()> {
    let hostname = get_hostname();
    let config = crate::config::root_config()?;
    let hex_config = hexpm::Config::new();
    let http = HttpClient::new();

    // Build the project to check that it is valid
    let _ = build::main()?;

    // Get login creds from user
    let username = cli::ask("https://hex.pm username")?;
    let password = cli::ask_password("https://hex.pm password")?;

    // Create API token
    let key = gleam_core::hex::create_api_key(&hostname, &username, &password, &hex_config, &http)
        .await?;

    // TODO: Build HTML documentation

    tracing::info!("Creating release tarball");
    let _tarball = contents_tarball()?;
    let _version_int = package_version_int(&config.name).await?;

    // TODO: Build release tarball
    // https://github.com/hexpm/specifications/blob/master/package_tarball.md

    // TODO: Publish release to hexpm

    // TODO: Publish docs to hexpm for release

    // Delete API token
    gleam_core::hex::remove_api_key(&hostname, &hex_config, &key, &http).await?;

    Ok(())
}

/// Hex wants a second version number that is a single int. It's unclear why,
/// and we don't know what this number should be locally, so use the number of
/// releases as that int.
async fn package_version_int(name: &str) -> Result<usize> {
    let config = hexpm::Config::new();
    let request = hexpm::get_package_request(name, None, &config);
    let response = HttpClient::new().send(request).await?;
    let int = match hexpm::get_package_response(response, HEXPM_PUBLIC_KEY) {
        Ok(response) => response.releases.len(),
        Err(hexpm::ApiError::NotFound) => 0, // Not found
        Err(e) => return Err(Error::hex(e)),
    };
    tracing::debug!(version=%int, "Package int version fetched");
    Ok(int)
}

fn contents_tarball() -> Result<Vec<u8>, Error> {
    let mut contents_tar_gz = Vec::new();
    {
        let mut tarball =
            tar::Builder::new(GzEncoder::new(&mut contents_tar_gz, Compression::default()));
        for path in fs::gleam_files(&PathBuf::from("src")) {
            add_to_tar(&mut tarball, path)?;
        }
        add_to_tar(&mut tarball, "gleam.toml")?;
        tarball.finish().map_err(Error::finish_tar)?;
    }
    Ok(contents_tar_gz)
}

fn add_to_tar<P, W>(tarball: &mut tar::Builder<W>, path: P) -> Result<()>
where
    P: AsRef<Path>,
    W: Write,
{
    let path = path.as_ref();
    tracing::debug!(file=?path, "Adding file to tarball");
    tarball
        .append_path(path)
        .map_err(|e| Error::add_tar(path, e))
}

#[derive(Debug, Clone)]
pub struct ReleaseMetadata {
    name: String,
    version: String,
    app: String,
    description: String,
    files: Vec<String>,
    licenses: Vec<String>,
    maintainers: Vec<String>,
    links: Vec<(String, http::Uri)>,
    requirements: Vec<ReleaseRequirement>,
    build_tools: Vec<String>,
    // What should this be? I can't find it in the API anywhere.
    // extra: (kvlist(string => kvlist(...))) (optional)
}

#[derive(Debug, Clone)]
struct ReleaseRequirement {
    app: String,
    optional: String,
    requirement: String,
    // What values are valid for this?
    // repository: String,
}

fn get_hostname() -> String {
    hostname::get()
        .expect("Looking up hostname")
        .to_string_lossy()
        .to_string()
}
