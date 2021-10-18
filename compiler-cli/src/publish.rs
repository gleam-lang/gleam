use std::path::{Path, PathBuf};

use flate2::{write::GzEncoder, Compression};
use gleam_core::{Error, Result};

use crate::{build, fs};

pub fn command() -> Result<()> {
    let _config = crate::config::root_config()?;

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
    // TODO: Build release tarball
    // https://github.com/hexpm/specifications/blob/master/package_tarball.md

    // TODO: Publish release to hexpm

    // TODO: Publish docs to hexpm for release

    // TODO: Delete API token

    Ok(())
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

fn add_to_tar<P>(tarball: &mut tar::Builder<GzEncoder<&mut Vec<u8>>>, path: P) -> Result<()>
where
    P: AsRef<Path>,
{
    let path = path.as_ref();
    tracing::debug!(file=?path, "Adding file to tarball");
    tarball
        .append_path(path)
        .map_err(|e| Error::add_tar(path, e))
}
