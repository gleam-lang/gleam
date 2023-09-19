use camino::Utf8PathBuf;
use gleam_core::{
    error::{FileIoAction, FileKind},
    Error, Result,
};
use std::str::FromStr;

pub fn run(files: Vec<String>) -> Result<()> {
    for file_path in files {
        let path = Utf8PathBuf::from_str(&file_path).map_err(|e| Error::FileIo {
            action: FileIoAction::Open,
            kind: FileKind::File,
            path: Utf8PathBuf::from(file_path),
            err: Some(e.to_string()),
        })?;

        if path.is_dir() {
            for path in crate::fs::gleam_files_excluding_gitignore(&path) {
                fix_file(path)?;
            }
        } else {
            fix_file(path)?;
        }
    }

    println!("All fixed!");
    Ok(())
}

fn fix_file(path: Utf8PathBuf) -> Result<()> {
    let src = crate::fs::read(&path)?;
    let out = gleam_core::fix::parse_fix_and_format(&src.into(), &path)?;
    crate::fs::write(&path, &out)?;
    Ok(())
}
