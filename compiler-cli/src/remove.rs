use gleam_core::{
    Error, Result,
    error::{FileIoAction, FileKind},
    paths::ProjectPaths,
};

use crate::{cli, fs};

pub fn command(paths: &ProjectPaths, packages: Vec<String>) -> Result<()> {
    // Read gleam.toml so we can remove deps from it
    let root_config = paths.root_config();
    let mut toml = fs::read(&root_config)?
        .parse::<toml_edit::DocumentMut>()
        .map_err(|e| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Parse,
            path: root_config.to_path_buf(),
            err: Some(e.to_string()),
        })?;

    // Remove the specified dependencies
    let mut packages_not_exist = vec![];
    for package_to_remove in packages.iter() {
        let remove = |toml: &mut toml_edit::DocumentMut, name| {
            #[allow(clippy::indexing_slicing)]
            toml[name]
                .as_table_like_mut()
                .and_then(|deps| deps.remove(package_to_remove))
        };

        // dev-dependencies is the old deprecated name for dev_dependencies
        let removed = remove(&mut toml, "dependencies")
            .or_else(|| remove(&mut toml, "dev_dependencies"))
            .or_else(|| remove(&mut toml, "dev-dependencies"));

        if removed.is_none() {
            packages_not_exist.push(package_to_remove.into());
        }
    }

    if !packages_not_exist.is_empty() {
        return Err(Error::RemovedPackagesNotExist {
            packages: packages_not_exist,
        });
    }

    // Write the updated config
    fs::write(root_config.as_path(), &toml.to_string())?;
    _ = crate::dependencies::cleanup(paths, cli::Reporter::new())?;

    Ok(())
}
