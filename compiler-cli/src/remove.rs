use std::path::{Path, PathBuf};

use gleam_core::{
    error::{FileIoAction, FileKind},
    Error, Result,
};

use crate::{cli, fs, UseManifest};

pub fn command(packages: Vec<String>) -> Result<()> {
    // Read gleam.toml so we can remove deps from it
    let mut toml = fs::read("gleam.toml")?
        .parse::<toml_edit::Document>()
        .map_err(|e| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Parse,
            path: PathBuf::from("gleam.toml"),
            err: Some(e.to_string()),
        })?;

    // Remove the specified dependencies
    for package_to_remove in packages.iter() {
        #[allow(clippy::indexing_slicing)]
        let _ = toml["dependencies"]
            .as_table_mut()
            .and_then(|deps| deps.remove(package_to_remove));
        #[allow(clippy::indexing_slicing)]
        let _ = toml["dev-dependencies"]
            .as_table_mut()
            .and_then(|deps| deps.remove(package_to_remove));
    }

    // Write the updated config
    fs::write(Path::new("gleam.toml"), &toml.to_string())?;
    let paths = crate::project_paths_at_current_directory();
    _ = crate::dependencies::download(&paths, cli::Reporter::new(), None, UseManifest::Yes)?;
    for package_to_remove in packages {
        cli::print_removed(&package_to_remove);
    }

    Ok(())
}
