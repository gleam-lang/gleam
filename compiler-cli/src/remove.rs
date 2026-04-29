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

    // If there is no manifest, there is no dependency state to clean up.
    // This can happen in a newly created project or while users are temporarily
    // recreating their manifest. Removing the entry from gleam.toml is enough.
    if !paths.manifest().exists() {
        return Ok(());
    }

    _ = crate::dependencies::cleanup(paths, cli::Reporter::new())?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use gleam_core::paths::ProjectPaths;
    use pretty_assertions::assert_eq;

    use super::command;

    #[test]
    fn remove_dependency_without_manifest() {
        let tmp = tempfile::tempdir().unwrap();
        let root = Utf8PathBuf::from_path_buf(tmp.path().to_path_buf()).unwrap();
        let paths = ProjectPaths::new(root.clone());
        std::fs::write(
            root.join("gleam.toml"),
            r#"name = "app"
version = "1.0.0"

[dependencies]
gleam_stdlib = ">= 0.44.0 and < 2.0.0"
gleeunit = ">= 1.0.0 and < 2.0.0"
"#,
        )
        .unwrap();

        command(&paths, vec!["gleeunit".to_string()]).unwrap();

        assert_eq!(
            std::fs::read_to_string(root.join("gleam.toml")).unwrap(),
            r#"name = "app"
version = "1.0.0"

[dependencies]
gleam_stdlib = ">= 0.44.0 and < 2.0.0"
"#
        );
    }
}
