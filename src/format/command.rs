use crate::error::{Error, FileIOAction, FileKind};
use std::fs::OpenOptions;
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::PathBuf;
use std::str::FromStr;

pub fn run(stdin: bool, check: bool, files: Vec<String>, root_path: String) -> Result<(), Error> {
    if stdin {
        format_stdin(check)
    } else {
        format_files(files, check, root_path)
    }
}

pub fn format_files(files: Vec<String>, _check: bool, root_path: String) -> Result<(), Error> {
    for file_path in files {
        let path = PathBuf::from_str(&root_path)
            .map_err(|e| Error::FileIO {
                action: FileIOAction::Open,
                kind: FileKind::File,
                path: PathBuf::new(),
                err: Some(format!("Invalid path: {}\n{}", file_path, e.to_string())),
            })?
            .join(&file_path);

        let mut file = OpenOptions::new()
            .write(true)
            .read(true)
            .open(path.clone())
            .expect(&format!("Could not open file: {}", file_path));

        let mut src = String::new();
        file.read_to_string(&mut src)
            .expect(&format!("Could not read file: {}", file_path));

        let formatted = crate::format::pretty(src.as_ref()).map_err(|error| Error::Parse {
            path: PathBuf::from("<standard input>"),
            src: src.clone(),
            error,
        })?;

        file.seek(SeekFrom::Start(0)).unwrap();
        file.set_len(0).unwrap();

        file.write(&mut formatted.as_bytes())
            .map_err(|e| Error::FileIO {
                action: FileIOAction::WriteTo,
                kind: FileKind::File,
                err: Some(e.to_string()),
                path,
            })?;
    }

    Ok(())
}

pub fn format_stdin(_check: bool) -> Result<(), Error> {
    let mut src = String::new();
    std::io::stdin()
        .read_to_string(&mut src)
        .expect("Reading stdin");

    let formatted = crate::format::pretty(src.as_ref()).map_err(|error| Error::Parse {
        path: PathBuf::from("<standard input>"),
        error,
        src,
    })?;

    print!("{}", formatted);
    Ok(())
}
