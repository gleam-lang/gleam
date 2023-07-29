use camino::Utf8PathBuf;
use gleam_core::{
    build::Target,
    error::{FileIoAction, FileKind},
    Error, Result,
};
use std::str::FromStr;

pub fn run(target: Option<Target>, files: Vec<String>) -> Result<()> {
    let mut complete = true;

    for file_path in files {
        let path = Utf8PathBuf::from_str(&file_path).map_err(|e| Error::FileIo {
            action: FileIoAction::Open,
            kind: FileKind::File,
            path: Utf8PathBuf::from(file_path),
            err: Some(e.to_string()),
        })?;

        if path.is_dir() {
            for path in crate::fs::gleam_files_excluding_gitignore(&path) {
                let file_complete = fix_file(target, path)?;
                complete = complete && file_complete;
            }
        } else {
            let file_complete = fix_file(target, path)?;
            complete = complete && file_complete;
        }
    }

    if !complete {
        return Err(Error::AmbiguousExternalFnTarget);
    }

    println!("All fixed!");
    Ok(())
}

fn fix_file(target: Option<Target>, path: Utf8PathBuf) -> Result<bool> {
    let src = crate::fs::read(&path)?;
    let (out, complete) = gleam_core::fix::parse_fix_and_format(target, &src.into(), &path)?;
    crate::fs::write(&path, &out)?;
    Ok(complete)
}
