use crate::{
    error::{Error, FileIOAction, FileKind, StandardIOAction},
    file,
};
use std::{
    fs::File,
    io::{Read, Write},
    path::PathBuf,
    str::FromStr,
};

use ignore::Walk;

#[derive(Debug, PartialEq)]
pub struct Formatted {
    pub source: PathBuf,
    pub destination: Destination,
    pub input: String,
    pub output: String,
}

#[derive(Debug, PartialEq)]
pub enum Destination {
    Stdout,
    File { path: PathBuf },
}

pub fn run(stdin: bool, check: bool, files: Vec<String>) -> Result<(), Error> {
    let formatted = if stdin {
        vec![read_and_format_stdin()?]
    } else {
        read_and_format_paths(files)?
    };
    if check {
        check_formatting(formatted)
    } else {
        write_formatted(formatted)
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
        match formatted {
            Formatted {
                destination: Destination::Stdout,
                output,
                ..
            } => {
                print!("{}", output);
            }
            Formatted {
                destination: Destination::File { path },
                output,
                ..
            } => {
                let mut f = File::create(&path).map_err(|e| Error::FileIO {
                    action: FileIOAction::Create,
                    kind: FileKind::File,
                    path: path.clone(),
                    err: Some(e.to_string()),
                })?;

                f.write_all(output.as_bytes()).map_err(|e| Error::FileIO {
                    action: FileIOAction::WriteTo,
                    kind: FileKind::File,
                    path: path.clone(),
                    err: Some(e.to_string()),
                })?;
            }
        }
    }

    Ok(())
}

pub fn read_and_format_paths(files: Vec<String>) -> Result<Vec<Formatted>, Error> {
    let mut formatted_files = Vec::with_capacity(files.len());
    let mut visible_files_from_git = Vec::new();

    for result in Walk::new("./") {
        // Each item yielded by the iterator is either a directory entry or an
        // error, so either print the path or the error.
        match result {
            Ok(entry) => visible_files_from_git.push(entry.into_path()),
            Err(err) => println!("ERROR: {}", err),
        }
    }

    for file_path in files {
        let path = PathBuf::from_str(&file_path).map_err(|e| Error::FileIO {
            action: FileIOAction::Open,
            kind: FileKind::File,
            path: PathBuf::from(file_path),
            err: Some(e.to_string()),
        })?;

        if path.is_dir() {
            for current_path in file::gleam_files(&path).into_iter() {
                if visible_files_from_git.contains(&current_path) {
                    formatted_files.push(format_file(current_path)?);
                }
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
        source: path.clone(),
        destination: Destination::File { path },
        input: src,
        output: formatted,
    })
}

pub fn read_and_format_stdin() -> Result<Formatted, Error> {
    let mut src = String::new();
    std::io::stdin()
        .read_to_string(&mut src)
        .map_err(|e| Error::StandardIO {
            action: StandardIOAction::Read,
            err: Some(e.kind()),
        })?;

    let formatted = crate::format::pretty(src.as_ref()).map_err(|error| Error::Parse {
        path: PathBuf::from("<standard input>"),
        src: src.clone(),
        error,
    })?;
    Ok(Formatted {
        source: PathBuf::from("<standard input>"),
        destination: Destination::Stdout,
        input: src,
        output: formatted,
    })
}
