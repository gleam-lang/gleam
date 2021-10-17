use std::path::PathBuf;

use gleam_core::Result;

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

    // Create tarball
    let mut bytes = Vec::new();
    {
        let mut tarball = tar::Builder::new(&mut bytes);
        tarball.append_dir_all("src", "src").unwrap(); // TODO: error handling
        tarball.append_path("gleam.toml").unwrap(); // TODO: error handling
        tarball.finish().unwrap();
    }

    // TODO: Publish release to hexpm

    // TODO: Publish docs to hexpm for release

    // TODO: Delete API token

    Ok(())
}
