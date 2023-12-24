use camino::{Utf8Path, Utf8PathBuf};

use gleam_core::{
    error::{FileIoAction, FileKind},
    Error, Result,
};

use crate::{cli, dependencies::UseManifest, fs};

pub fn command(packages: Vec<String>, dev: bool) -> Result<()> {
    let paths = crate::project_paths_at_current_directory();

    // Insert the new packages into the manifest and perform dependency
    // resolution to determine suitable versions
    let manifest = crate::dependencies::download(
        &paths,
        cli::Reporter::new(),
        Some((packages.to_vec(), dev)),
        UseManifest::Yes,
    )?;

    // Read gleam.toml and manifest.toml so we can insert new deps into it
    let mut gleam_toml = read_toml_edit("gleam.toml")?;
    let mut manifest_toml = read_toml_edit("manifest.toml")?;

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

        // False positive. This package doesn't use the indexing API correctly.
        #[allow(clippy::indexing_slicing)]
        {
            if dev {
                gleam_toml["dev-dependencies"][&package_to_add] = toml_edit::value(range.clone());
            } else {
                gleam_toml["dependencies"][&package_to_add] = toml_edit::value(range.clone());
            };
            manifest_toml["requirements"][&package_to_add]
                .as_inline_table_mut()
                .expect("Invalid manifest format")["version"] = range.into();
        }

        cli::print_added(&format!("{package_to_add} v{version}"));
    }

    // Write the updated config
    fs::write(Utf8Path::new("gleam.toml"), &gleam_toml.to_string())?;
    fs::write(Utf8Path::new("manifest.toml"), &manifest_toml.to_string())?;

    Ok(())
}

fn read_toml_edit(name: &str) -> Result<toml_edit::Document, Error> {
    fs::read(name)?
        .parse::<toml_edit::Document>()
        .map_err(|e| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Parse,
            path: Utf8PathBuf::from("gleam.toml"),
            err: Some(e.to_string()),
        })
}
