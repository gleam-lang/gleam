use std::path::{Path, PathBuf};

use gleam_core::{
    error::{FileIoAction, FileKind},
    Error, Result,
};

use crate::fs;

pub fn command(to_add: String, dev: bool) -> Result<()> {
    // Insert the new package into the manifest and perform dependency
    // resolution to determine a suitable version
    let manifest = crate::dependencies::download(Some((&to_add, dev)))?;

    // Pull the selected version out of the new manifest so we know what it is
    let version = manifest
        .packages
        .into_iter()
        .find(|package| package.name == to_add)
        .expect("Added package not found in resolved manifest")
        .version;

    tracing::info!(version=%version, "new_package_version_resolved");

    // Produce a version requirement locked to the major version.
    // i.e. if 1.2.3 is selected we want ~> 1.2
    let range = format!("~> {}.{}", version.major, version.minor);

    // Read gleam.toml so we can insert the new dep into it
    let mut toml = fs::read("gleam.toml")?
        .parse::<toml_edit::Document>()
        .map_err(|e| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Parse,
            path: PathBuf::from("gleam.toml"),
            err: Some(e.to_string()),
        })?;

    // Insert the new dep
    if dev {
        toml["dev-dependencies"][to_add] = toml_edit::value(range);
    } else {
        toml["dependencies"][to_add] = toml_edit::value(range);
    };

    // Write the updated config
    fs::write(Path::new("gleam.toml"), &toml.to_string())?;

    Ok(())
}
