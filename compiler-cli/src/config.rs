use std::{collections::HashMap, path::PathBuf};

use crate::dependencies::Manifest;
use gleam_core::{
    config::PackageConfig,
    error::{Error, FileIoAction, FileKind},
    paths,
};

pub fn root_config() -> Result<PackageConfig, Error> {
    read_project_config(paths::root_config())
}

pub fn package_configs(
    root_name: &str,
    manifest: &Manifest,
) -> Result<HashMap<String, PackageConfig>, Error> {
    let mut configs = HashMap::new();
    for package in &manifest.packages {
        if &package.name == root_name {
            continue;
        }
        let config = read_project_config(paths::build_deps_package_config(&package.name))?;
        let _ = configs.insert(config.name.clone(), config);
    }
    Ok(configs)
}

pub fn read_project_config(config_path: PathBuf) -> Result<PackageConfig, Error> {
    let toml = crate::fs::read(&config_path)?;
    toml::from_str(&toml).map_err(|e| Error::FileIo {
        action: FileIoAction::Parse,
        kind: FileKind::File,
        path: config_path,
        err: Some(e.to_string()),
    })
}
