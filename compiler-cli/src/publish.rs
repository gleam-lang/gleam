use std::{
    io::Write,
    path::{Path, PathBuf},
};

use flate2::{write::GzEncoder, Compression};
use gleam_core::{hex::HEXPM_PUBLIC_KEY, io::HttpClient as _, Error, Result};

use crate::{build, fs, http::HttpClient};

pub fn command() -> Result<()> {
    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
    let config = crate::config::root_config()?;

    // Build the project to check that it is valid
    let _ = build::main()?;

    // TODO: Get hex username from user

    // TODO: Get hex password from user

    // TODO: Create API token

    // TODO: Build HTML documentation

    // TODO: Build HTML documentation

    // TODO: Read project files

    tracing::info!("Creating release tarball");
    let _tarball = contents_tarball()?;
    let _version_int = runtime.block_on(package_version_int(&config.name))?;
    // TODO: Build release tarball
    // https://github.com/hexpm/specifications/blob/master/package_tarball.md

    // TODO: Publish release to hexpm

    // TODO: Publish docs to hexpm for release

    // TODO: Delete API token

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
