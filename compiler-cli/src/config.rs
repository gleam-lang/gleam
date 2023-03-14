use std::path::Path;

use gleam_core::{
    config::PackageConfig,
    error::{Error, FileIoAction, FileKind},
    paths::ProjectPaths,
};

pub fn root_config(paths: &ProjectPaths) -> Result<PackageConfig, Error> {
    read(paths.root_config())
}

pub fn read(config_path: &Path) -> Result<PackageConfig, Error> {
    let toml = crate::fs::read(config_path)?;
    toml::from_str(&toml).map_err(|e| Error::FileIo {
        action: FileIoAction::Parse,
        kind: FileKind::File,
        path: config_path.to_path_buf(),
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
