use crate::{
    error::{Error, FileIOAction, FileKind, Result, StandardIOAction},
    fs::OutputFile,
};
use std::{io::Read, path::PathBuf, str::FromStr};

#[derive(Debug, PartialEq)]
pub struct Unformatted {
    pub source: PathBuf,
    pub destination: PathBuf,
    pub input: String,
    pub output: String,
}

pub fn run(stdin: bool, check: bool, files: Vec<String>) -> Result<()> {
    if stdin {
        process_stdin(check)
    } else {
        process_files(check, files)
    }
}

fn process_stdin(check: bool) -> Result<()> {
    let src = read_stdin()?;
    let mut out = String::new();
    crate::format::pretty(&mut out, src.as_str())?;

    if !check {
        print!("{}", out);
        return Ok(());
    }

    if src != out {
        return Err(Error::Format {
            problem_files: vec![Unformatted {
                source: PathBuf::from("<standard input>"),
                destination: PathBuf::from("<standard output>"),
                input: src,
                output: out,
            }],
        });
    }

    Ok(())
}

fn process_files(check: bool, files: Vec<String>) -> Result<()> {
    if check {
        check_files(files)
    } else {
        format_files(files)
    }
}

fn check_files(files: Vec<String>) -> Result<()> {
    let problem_files = unformatted_files(files)?;

    if problem_files.is_empty() {
        Ok(())
    } else {
        Err(Error::Format { problem_files })
    }
}

fn format_files(files: Vec<String>) -> Result<()> {
    for file in unformatted_files(files)?.into_iter() {
        crate::fs::write_output(&OutputFile {
            path: file.destination,
            text: file.output,
        })?;
    }
    Ok(())
}

pub fn unformatted_files(files: Vec<String>) -> Result<Vec<Unformatted>> {
    let mut problem_files = Vec::with_capacity(files.len());

    for file_path in files {
        let path = PathBuf::from_str(&file_path).map_err(|e| Error::FileIO {
            action: FileIOAction::Open,
            kind: FileKind::File,
            path: PathBuf::from(file_path),
            err: Some(e.to_string()),
        })?;

        if path.is_dir() {
            for path in crate::fs::gleam_files_excluding_gitignore(&path).into_iter() {
                format_file(&mut problem_files, path)?;
            }
        } else {
            format_file(&mut problem_files, path)?;
        }
    }

    Ok(problem_files)
}

fn format_file(problem_files: &mut Vec<Unformatted>, path: PathBuf) -> Result<()> {
    let src = crate::fs::read(&path)?;
    let mut output = String::new();
    crate::format::pretty(&mut output, src.as_str())?;

    if src != output {
        problem_files.push(Unformatted {
            source: path.clone(),
            destination: path,
            input: src,
            output,
        });
    }
    Ok(())
}

pub fn read_stdin() -> Result<String> {
    let mut src = String::new();
    std::io::stdin()
        .read_to_string(&mut src)
        .map_err(|e| Error::StandardIO {
            action: StandardIOAction::Read,
            err: Some(e.kind()),
        })?;
    Ok(src)
}
