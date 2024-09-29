use gleam_core::{
    error::{Error, FileIoAction, FileKind, Result, StandardIoAction, Unformatted},
    io::{Content, OutputFile},
    sourcemap::SourceMapEmitter,
};
use std::{io::Read, str::FromStr};

use camino::{Utf8Path, Utf8PathBuf};

pub fn run(stdin: bool, check: bool, files: Vec<String>) -> Result<()> {
    if stdin {
        process_stdin(check)
    } else {
        process_files(check, files)
    }
}

fn process_stdin(check: bool) -> Result<()> {
    let src = read_stdin()?.into();
    let mut out = String::new();
    gleam_core::format::pretty(
        &mut out,
        &src,
        Utf8Path::new("<stdin>"),
        &mut SourceMapEmitter::null(),
    )?;

    if !check {
        print!("{out}");
        return Ok(());
    }

    if src != out {
        return Err(Error::Format {
            problem_files: vec![Unformatted {
                source: Utf8PathBuf::from("<standard input>"),
                destination: Utf8PathBuf::from("<standard output>"),
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
    for file in unformatted_files(files)? {
        crate::fs::write_output(&OutputFile {
            path: file.destination,
            content: Content::Text(file.output),
        })?;
    }
    Ok(())
}

pub fn unformatted_files(files: Vec<String>) -> Result<Vec<Unformatted>> {
    let mut problem_files = Vec::with_capacity(files.len());

    for file_path in files {
        let path = Utf8PathBuf::from_str(&file_path).map_err(|e| Error::FileIo {
            action: FileIoAction::Open,
            kind: FileKind::File,
            path: Utf8PathBuf::from(file_path),
            err: Some(e.to_string()),
        })?;

        if path.is_dir() {
            for path in crate::fs::gleam_files_excluding_gitignore(&path) {
                format_file(&mut problem_files, path)?;
            }
        } else {
            format_file(&mut problem_files, path)?;
        }
    }

    Ok(problem_files)
}

fn format_file(problem_files: &mut Vec<Unformatted>, path: Utf8PathBuf) -> Result<()> {
    let src = crate::fs::read(&path)?.into();
    let mut output = String::new();
    gleam_core::format::pretty(&mut output, &src, &path, &mut SourceMapEmitter::null())?;

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
    let _ = std::io::stdin()
        .read_to_string(&mut src)
        .map_err(|e| Error::StandardIo {
            action: StandardIoAction::Read,
            err: Some(e.kind()),
        })?;
    Ok(src)
}
