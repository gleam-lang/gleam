use gleam_core::{
    build::Target,
    error::{FileIoAction, FileKind},
    Error, Result,
};
use std::{path::PathBuf, str::FromStr};

pub fn run(target: Option<Target>, files: Vec<String>) -> Result<()> {
    for file_path in files {
        let path = PathBuf::from_str(&file_path).map_err(|e| Error::FileIo {
            action: FileIoAction::Open,
            kind: FileKind::File,
            path: PathBuf::from(file_path),
            err: Some(e.to_string()),
        })?;

        if path.is_dir() {
            for path in crate::fs::gleam_files_excluding_gitignore(&path) {
                fix_file(target, path)?;
            }
        } else {
            fix_file(target, path)?;
        }
    }
    Ok(())
}

fn fix_file(target: Option<Target>, path: PathBuf) -> Result<()> {
    let src = crate::fs::read(&path)?;
    let out = gleam_core::fix::parse_fix_and_format(target, &src.into(), &path)?;
    crate::fs::write(&path, &out)?;
    Ok(())
}
