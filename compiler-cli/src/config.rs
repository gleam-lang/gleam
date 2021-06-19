use std::{collections::HashMap, path::Path};

use gleam_core::{
    build::project_root::ProjectRoot,
    config::PackageConfig,
    error::{Error, FileIoAction, FileKind},
};

pub fn root_config(root: &ProjectRoot) -> Result<PackageConfig, Error> {
    read_project_config(&root.root)
}

pub fn package_configs(
    root: &ProjectRoot,
    root_name: &str,
) -> Result<HashMap<String, PackageConfig>, Error> {
    let mut configs = HashMap::with_capacity(25);
    for dir_entry in crate::fs::read_dir(root.default_build_lib_path())?.filter_map(Result::ok) {
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
