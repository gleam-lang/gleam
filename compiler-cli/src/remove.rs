use camino::{Utf8Path, Utf8PathBuf};

use gleam_core::{
    error::{FileIoAction, FileKind},
    Error, Result,
};

use crate::{cli, fs};

pub fn command(packages: Vec<String>) -> Result<()> {
    // Read gleam.toml so we can remove deps from it
    let mut toml = fs::read("gleam.toml")?
        .parse::<toml_edit::DocumentMut>()
        .map_err(|e| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Parse,
            path: Utf8PathBuf::from("gleam.toml"),
            err: Some(e.to_string()),
        })?;

    // Remove the specified dependencies
    let mut packages_not_exist = vec![];
    for package_to_remove in packages.iter() {
        #[allow(clippy::indexing_slicing)]
        let maybe_removed_item = toml["dependencies"]
            .as_table_like_mut()
            .and_then(|deps| deps.remove(package_to_remove));

        #[allow(clippy::indexing_slicing)]
        let maybe_removed_dev_item = toml["dev-dependencies"]
            .as_table_like_mut()
            .and_then(|deps| deps.remove(package_to_remove));

        if maybe_removed_item.or(maybe_removed_dev_item).is_none() {
            packages_not_exist.push(package_to_remove.into());
        }
    }

    if !packages_not_exist.is_empty() {
        return Err(Error::RemovedPackagesNotExist {
            packages: packages_not_exist,
        });
    }

    // Write the updated config
    fs::write(Utf8Path::new("gleam.toml"), &toml.to_string())?;
    let paths = crate::find_project_paths()?;

    _ = crate::dependencies::cleanup(&paths, cli::Reporter::new())?;

    for package_to_remove in packages {
        cli::print_removed(&package_to_remove);
    }

    Ok(())
}
