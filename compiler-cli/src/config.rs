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
            path: path.to_path_buf(),
            err: Some("File not found".into()),
        });
    }
    Ok(())
}
