use std::path::{Path, PathBuf};

use gleam_core::{
    error::{FileIoAction, FileKind},
    Error, Result,
};

use crate::{cli, fs};

pub fn command(packages: Vec<String>, dev: bool) -> Result<()> {
    // Insert the new packages into the manifest and perform dependency
    // resolution to determine suitable versions
    let manifest =
        crate::dependencies::download(cli::Reporter::new(), Some((packages.to_vec(), dev)))?;

    // Read gleam.toml so we can insert new deps into it
    let mut toml = fs::read("gleam.toml")?
        .parse::<toml_edit::Document>()
        .map_err(|e| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Parse,
            path: PathBuf::from("gleam.toml"),
            err: Some(e.to_string()),
        })?;

    // Insert the new deps
    for package_to_add in packages {
        // Pull the selected version out of the new manifest so we know what it is
        let version = &manifest
            .packages
            .iter()
            .find(|package| package.name == *package_to_add)
            .expect("Added package not found in resolved manifest")
            .version;

        tracing::info!(version=%version, "new_package_version_resolved");

        // Produce a version requirement locked to the major version.
        // i.e. if 1.2.3 is selected we want ~> 1.2
        let range = format!("~> {}.{}", version.major, version.minor);

        #[allow(clippy::indexing_slicing)]
        if dev {
            toml["dev-dependencies"][&package_to_add] = toml_edit::value(range);
        } else {
            toml["dependencies"][&package_to_add] = toml_edit::value(range);
        };

        cli::print_added(&format!("{} v{}", package_to_add, version));
    }

    // Write the updated config
    fs::write(Path::new("gleam.toml"), &toml.to_string())?;

    Ok(())
}
