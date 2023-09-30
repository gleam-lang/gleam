use camino::Utf8PathBuf;

use gleam_core::{
    config::PackageConfig,
    error::{Error, FileIoAction, FileKind},
    manifest::Manifest,
    paths::ProjectPaths,
};
use smol_str::SmolStr;

use crate::fs::get_current_directory;

pub fn root_config() -> Result<PackageConfig, Error> {
    let current_dir = get_current_directory().expect("Failed to get current directory");
    let paths = ProjectPaths::new(current_dir);
    read(paths.root_config())
}

/// Get the config for a dependency module. Return the config for the current
/// project if a dependency doesn't have a config file.
pub fn find_package_config_for_module(
    mod_path: &str,
    manifest: &Manifest,
    project_paths: &ProjectPaths,
) -> Result<PackageConfig, Error> {
    let gleam_projects: Vec<SmolStr> = manifest
        .packages
        .iter()
        .filter(|package| package.build_tools.contains(&"gleam".to_string()))
        .map(|package| package.name.clone())
        .collect();

    let maybe_package_path = gleam_projects.into_iter().find(|package_to_check| {
        let mut path = project_paths.build_packages_directory();
        path.push(package_to_check.as_str());

        path.push("src");

        for file in mod_path.split('/') {
            path.push(file);
        }

        let _ = path.set_extension("gleam");

        path.is_file()
    });

    match maybe_package_path {
        Some(package_path) => {
            let mut config_path = project_paths.build_packages_directory();
            config_path.push(package_path.as_str());
            config_path.push("gleam.toml");
            read(config_path)
        }
        None => root_config(),
    }
}

pub fn read(config_path: Utf8PathBuf) -> Result<PackageConfig, Error> {
    let toml = crate::fs::read(&config_path)?;
    let config: PackageConfig = toml::from_str(&toml).map_err(|e| Error::FileIo {
        action: FileIoAction::Parse,
        kind: FileKind::File,
        path: config_path,
        err: Some(e.to_string()),
    })?;
    config.check_gleam_compatibility()?;
    Ok(config)
}

pub fn ensure_config_exists(paths: &ProjectPaths) -> Result<(), Error> {
    let path = paths.root_config();
    if !path.is_file() {
        return Err(Error::FileIo {
            action: FileIoAction::Read,
            kind: FileKind::File,
            path,
            err: Some("File not found".into()),
        });
    }
    Ok(())
}
