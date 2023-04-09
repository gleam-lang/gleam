use std::fs;
use std::path::PathBuf;

use gleam_core::{
    config::PackageConfig,
    error::{Error, FileIoAction, FileKind},
    paths::ProjectPaths,
};

pub fn root_config() -> Result<PackageConfig, Error> {
    let current_dir = std::env::current_dir().expect("Could not get current directory");
    let paths = ProjectPaths::new(current_dir);
    read(paths.root_config())
}

/// Get the config for a dependency module. Return the config for the current
/// project if a dependency doesn't have a config file.
pub fn module_config(mod_path: &str, project_paths: &ProjectPaths) -> Result<PackageConfig, Error> {
    let package_path = fs::read_dir(project_paths.build_packages_directory()).map_or(None, |x| {
        {
            x.filter_map(|file| match file {
                Ok(file) => Some(file),
                Err(_) => None,
            })
            .find(|file| {
                file.path()
                    .join("src")
                    .join(mod_path.to_string() + ".gleam")
                    .is_file()
            })
        }
    });

    println!("{:?}", package_path);

    match package_path {
        Some(file) => {
            let config_path = file.path().join("gleam.toml");
            read(config_path)
        }
        None => root_config(),
    }
}

pub fn read(config_path: PathBuf) -> Result<PackageConfig, Error> {
    let toml = crate::fs::read(&config_path)?;
    toml::from_str(&toml).map_err(|e| Error::FileIo {
        action: FileIoAction::Parse,
        kind: FileKind::File,
        path: config_path,
        err: Some(e.to_string()),
    })
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
