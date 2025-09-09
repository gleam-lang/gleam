use camino::{Utf8Path, Utf8PathBuf};

use gleam_core::{
    Error, Result,
    error::{FileIoAction, FileKind},
    io::HttpClient as HttpClientTrait,
    paths::ProjectPaths,
};

use crate::{
    cli,
    dependencies::{self, parse_gleam_add_specifier},
    fs,
    http::HttpClient,
};

pub fn command(paths: &ProjectPaths, packages_to_add: Vec<String>, dev: bool) -> Result<()> {
    let config = crate::config::root_config(paths)?;
    if packages_to_add.iter().any(|name| name == &config.name) {
        return Err(Error::CannotAddSelfAsDependency {
            name: config.name.clone(),
        });
    }

    let mut new_package_requirements = Vec::with_capacity(packages_to_add.len());
    for specifier in packages_to_add {
        new_package_requirements.push(parse_gleam_add_specifier(&specifier)?);
    }

    // Insert the new packages into the manifest and perform dependency
    // resolution to determine suitable versions
    let manifest = dependencies::download(
        paths,
        cli::Reporter::new(),
        Some((new_package_requirements.clone(), dev)),
        Vec::new(),
        dependencies::DependencyManagerConfig {
            use_manifest: dependencies::UseManifest::Yes,
            check_major_versions: dependencies::CheckMajorVersions::No,
        },
    )?;

    // Read gleam.toml and manifest.toml so we can insert new deps into it
    let mut gleam_toml = read_toml_edit(&paths.root_config())?;
    let mut manifest_toml = read_toml_edit(&paths.manifest())?;

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
                if !gleam_toml.as_table().contains_key("dev-dependencies") {
                    gleam_toml["dev-dependencies"] = toml_edit::table();
                }
                gleam_toml["dev-dependencies"][&added_package] = toml_edit::value(range.clone());
            } else {
                if !gleam_toml.as_table().contains_key("dependencies") {
                    gleam_toml["dependencies"] = toml_edit::table();
                }
                gleam_toml["dependencies"][&added_package] = toml_edit::value(range.clone());
            };
            manifest_toml["requirements"][&added_package]
                .as_inline_table_mut()
                .expect("Invalid manifest format")["version"] = range.into();
        }

        cli::print_added(&format!("{added_package} v{version}"));

        let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio async runtime");
        let hex_config = hexpm::Config::new();
        let api_key = crate::hex::HexAuthentication::new(&runtime, hex_config.clone())
            .get_or_create_api_key()?;
        let http = HttpClient::new();
        let request = hexpm::get_package_release_request(
            added_package.as_str(),
            version.to_string().as_str(),
            Some(api_key.as_str()),
            &hex_config,
        );
        let response = runtime.block_on(http.send(request))?;
        let parsed_package =
            hexpm::get_package_release_response(response).expect("Failed to parse package data");

        if let Some(status) = parsed_package.retirement_status {
            println!(
                "Warning: Package {} {}.{}.{} is retired for reason: {}",
                added_package,
                version.major,
                version.minor,
                version.patch,
                status.reason.to_str()
            )
        }
    }

    // Write the updated config
    fs::write(&paths.root_config(), &gleam_toml.to_string())?;
    fs::write(&paths.manifest(), &manifest_toml.to_string())?;

    Ok(())
}

fn read_toml_edit(name: &Utf8Path) -> Result<toml_edit::DocumentMut, Error> {
    fs::read(name)?
        .parse::<toml_edit::DocumentMut>()
        .map_err(|e| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Parse,
            path: Utf8PathBuf::from("gleam.toml"),
            err: Some(e.to_string()),
        })
}
