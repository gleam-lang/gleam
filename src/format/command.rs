use crate::error::Error;
use std::io::Read;
use std::path::PathBuf;

pub fn run(stdin: bool, check: bool, files: Vec<String>) -> Result<(), Error> {
    if stdin {
        format_stdin(check)
    } else {
        format_files(files, check)
    }
}

pub fn format_files(_files: Vec<String>, _check: bool) -> Result<(), Error> {
    todo!()
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
