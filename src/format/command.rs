use crate::error::{Error, FileIOAction, FileKind, StandardIOAction};
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub struct Formatted {
    pub path: PathBuf,
    pub input: String,
    pub output: String,
}

pub fn run(stdin: bool, check: bool, files: Vec<String>) -> Result<(), Error> {
    if stdin {
        format_stdin(check)
    } else {
        let formatted = read_and_format_paths(files)?;
        if check {
            check_formatting(formatted)
        } else {
            write_formatted(formatted)
        }
    }
}

fn check_formatting(formatted_files: Vec<Formatted>) -> Result<(), Error> {
    let problem_files: Vec<_> = formatted_files
        .into_iter()
        .filter(|formatted| formatted.input != formatted.output)
        .collect();

    if problem_files.is_empty() {
        Ok(())
    } else {
        Err(Error::Format { problem_files })
    }
}

fn write_formatted(formatted_files: Vec<Formatted>) -> Result<(), Error> {
    for formatted in formatted_files {
        let path = formatted.path;
        let mut f = File::create(&path).map_err(|e| Error::FileIO {
            action: FileIOAction::Create,
            kind: FileKind::File,
            path: path.clone(),
            err: Some(e.to_string()),
        })?;

        f.write_all(formatted.output.as_bytes())
            .map_err(|e| Error::FileIO {
                action: FileIOAction::WriteTo,
                kind: FileKind::File,
                path: path.clone(),
                err: Some(e.to_string()),
            })?;
    }

    Ok(())
}

pub fn read_and_format_paths(files: Vec<String>) -> Result<Vec<Formatted>, Error> {
    let mut formatted_files = Vec::with_capacity(files.len());

    for file_path in files {
        let path = PathBuf::from_str(&file_path).map_err(|e| Error::FileIO {
            action: FileIOAction::Open,
            kind: FileKind::File,
            path: PathBuf::from(file_path),
            err: Some(e.to_string()),
        })?;

        if path.is_dir() {
            for path in crate::project::gleam_files(&path).into_iter() {
                formatted_files.push(format_file(path)?);
            }
        } else {
            formatted_files.push(format_file(path)?);
        }
    }

    Ok(formatted_files)
}

fn format_file(path: PathBuf) -> Result<Formatted, Error> {
    let src = std::fs::read_to_string(&path).map_err(|e| Error::FileIO {
        action: FileIOAction::Read,
        kind: FileKind::File,
        path: path.clone(),
        err: Some(e.to_string()),
    })?;

    let formatted = crate::format::pretty(src.as_ref()).map_err(|error| Error::Parse {
        path: path.clone(),
        src: src.clone(),
        error,
    })?;

    Ok(Formatted {
        path,
        input: src,
        output: formatted,
    })
}

pub fn format_stdin(_check: bool) -> Result<(), Error> {
    let mut src = String::new();
    std::io::stdin()
        .read_to_string(&mut src)
        .map_err(|e| Error::StandardIO {
            action: StandardIOAction::Read,
            err: Some(e.kind()),
        })?;

    let formatted = crate::format::pretty(src.as_ref()).map_err(|error| Error::Parse {
        path: PathBuf::from("<standard input>"),
        error,
        src,
    })?;

    print!("{}", formatted);
    Ok(())
}
