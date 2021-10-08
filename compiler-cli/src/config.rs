use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use gleam_core::{
    config::PackageConfig,
    error::{Error, FileIoAction, FileKind},
    paths,
};

pub fn root_config() -> Result<PackageConfig, Error> {
    read_project_config(PathBuf::from("./"))
}

pub fn package_configs(root_name: &str) -> Result<HashMap<String, PackageConfig>, Error> {
    let mut configs = HashMap::with_capacity(25);
    for dir_entry in crate::fs::read_dir(paths::build_deps())?.filter_map(Result::ok) {
        let config = read_project_config(dir_entry.path())?;
        if config.name != root_name {
            let _ = configs.insert(config.name.clone(), config);
        }
    }
    Ok(configs)
}

pub fn read_project_config(root: impl AsRef<Path>) -> Result<PackageConfig, Error> {
    let config_path = root.as_ref().join("gleam.toml");
    let toml = crate::fs::read(&config_path)?;
    toml::from_str(&toml).map_err(|e| Error::FileIo {
        action: FileIoAction::Parse,
        kind: FileKind::File,
        path: config_path.clone(),
        err: Some(e.to_string()),
    })
}
