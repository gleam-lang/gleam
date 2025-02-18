use camino::{Utf8Path, Utf8PathBuf};

use gleam_core::{
    error::{FileIoAction, FileKind},
    paths::ProjectPaths,
    Error, Result,
};

use crate::{
    cli,
    dependencies::{parse_gleam_add_specifier, UseManifest},
    fs,
};

pub fn command(paths: &ProjectPaths, packages_to_add: Vec<String>, dev: bool) -> Result<()> {
    let mut new_package_requirements = Vec::with_capacity(packages_to_add.len());
    for specifier in packages_to_add {
        new_package_requirements.push(parse_gleam_add_specifier(&specifier)?);
    }

    // Insert the new packages into the manifest and perform dependency
    // resolution to determine suitable versions
    let manifest = crate::dependencies::download(
        paths,
        cli::Reporter::new(),
        Some((new_package_requirements.clone(), dev)),
        Vec::new(),
        UseManifest::Yes,
    )?;

    // Read gleam.toml and manifest.toml so we can insert new deps into it
    let mut gleam_toml = read_toml_edit(paths.root_config().as_str())?;
    let mut manifest_toml = read_toml_edit(paths.manifest().as_str())?;

    // Insert the new deps
    for (added_package, _) in new_package_requirements {
        let added_package = added_package.to_string();

        // Pull the selected version out of the new manifest so we know what it is
        let version = &manifest
            .packages
            .iter()
            .find(|package| package.name == *added_package)
            .expect("Added package not found in resolved manifest")
            .version;

        tracing::info!(version=%version, "new_package_version_resolved");

        // Produce a version requirement locked to the major version.
        // i.e. if 1.2.3 is selected we want >= 1.2.3 and < 2.0.0
        let range = format!(
            ">= {}.{}.{} and < {}.0.0",
            version.major,
            version.minor,
            version.patch,
            version.major + 1
        );

        // False positive. This package doesn't use the indexing API correctly.
        #[allow(clippy::indexing_slicing)]
        {
            if dev {
                gleam_toml["dev-dependencies"][&added_package] = toml_edit::value(range.clone());
            } else {
                gleam_toml["dependencies"][&added_package] = toml_edit::value(range.clone());
            };
            manifest_toml["requirements"][&added_package]
                .as_inline_table_mut()
                .expect("Invalid manifest format")["version"] = range.into();
        }

        cli::print_added(&format!("{added_package} v{version}"));
    }

    // Write the updated config
    fs::write(
        Utf8Path::new(paths.root_config().as_str()),
        &gleam_toml.to_string(),
    )?;
    fs::write(
        Utf8Path::new(paths.manifest().as_str()),
        &manifest_toml.to_string(),
    )?;

    Ok(())
}

fn read_toml_edit(name: &str) -> Result<toml_edit::DocumentMut, Error> {
    fs::read(name)?
        .parse::<toml_edit::DocumentMut>()
        .map_err(|e| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Parse,
            path: Utf8PathBuf::from("gleam.toml"),
            err: Some(e.to_string()),
        })
}
