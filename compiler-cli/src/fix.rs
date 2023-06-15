use gleam_core::{
    build::Target,
    error::{FileIoAction, FileKind},
    Error, Result,
};
use std::{path::PathBuf, str::FromStr};

pub fn run(target: Option<Target>, files: Vec<String>) -> Result<()> {
    let mut complete = true;

    for file_path in files {
        let path = PathBuf::from_str(&file_path).map_err(|e| Error::FileIo {
            action: FileIoAction::Open,
            kind: FileKind::File,
            path: PathBuf::from(file_path),
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
        println!(
            "I wasn't able to determine what target should be used for all
external functions so they were not all converted to the new
syntax.

If you know what target should be used for all these ambiguous
functions run this command again with the --target flag set:

    gleam fix --target erlang
    gleam fix --target javascript
"
        );
        todo!("replace this with an actual error");
        std::process::exit(1);
    }

    println!("All fixed!");
    Ok(())
}

fn fix_file(target: Option<Target>, path: PathBuf) -> Result<bool> {
    let src = crate::fs::read(&path)?;
    let (out, complete) = gleam_core::fix::parse_fix_and_format(target, &src.into(), &path)?;
    crate::fs::write(&path, &out)?;
    Ok(complete)
}
