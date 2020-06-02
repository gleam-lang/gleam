use crate::error::{Error, FileIOAction, FileKind, GleamExpect};
use flate2::{write::GzEncoder, Compression};
use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
};

#[derive(Debug, PartialEq)]
pub struct OutputFile {
    pub text: String,
    pub path: PathBuf,
}

pub fn delete_dir(dir: &PathBuf) -> Result<(), Error> {
    if dir.exists() {
        std::fs::remove_dir_all(&dir).map_err(|e| Error::FileIO {
            action: FileIOAction::Delete,
            kind: FileKind::Directory,
            path: dir.clone(),
            err: Some(e.to_string()),
        })?;
    }
    Ok(())
}

pub fn write_outputs(outputs: &[OutputFile]) -> Result<(), Error> {
    for file in outputs {
        write_output(file)?;
    }
    Ok(())
}

pub fn write_output(file: &OutputFile) -> Result<(), Error> {
    let OutputFile { path, text } = file;

    let dir_path = path.parent().ok_or_else(|| Error::FileIO {
        action: FileIOAction::FindParent,
        kind: FileKind::Directory,
        path: path.clone(),
        err: None,
    })?;

    std::fs::create_dir_all(dir_path).map_err(|e| Error::FileIO {
        action: FileIOAction::Create,
        kind: FileKind::Directory,
        path: dir_path.to_path_buf(),
        err: Some(e.to_string()),
    })?;

    let mut f = File::create(&path).map_err(|e| Error::FileIO {
        action: FileIOAction::Create,
        kind: FileKind::File,
        path: path.clone(),
        err: Some(e.to_string()),
    })?;

    f.write_all(text.as_bytes()).map_err(|e| Error::FileIO {
        action: FileIOAction::WriteTo,
        kind: FileKind::File,
        path: path.clone(),
        err: Some(e.to_string()),
    })?;
    Ok(())
}

fn is_gleam_path(path: &PathBuf, dir: &PathBuf) -> bool {
    use regex::Regex;
    lazy_static! {
        static ref RE: Regex = Regex::new(
            format!(
                "^({module}{slash})*{module}\\.gleam$",
                module = "[a-z][_a-z0-9]*",
                slash = "(/|\\\\)",
            )
            .as_str()
        )
        .gleam_expect("is_gleam_path() RE regex");
    }

    RE.is_match(
        path.strip_prefix(dir)
            .gleam_expect("is_gleam_path(): strip_prefix")
            .to_str()
            .gleam_expect("is_gleam_path(): to_str"),
    )
}

#[test]
fn is_gleam_path_test() {
    assert!(is_gleam_path(
        &PathBuf::from("/some-prefix/a.gleam"),
        &PathBuf::from("/some-prefix/")
    ));

    assert!(is_gleam_path(
        &PathBuf::from("/some-prefix/one_two/a.gleam"),
        &PathBuf::from("/some-prefix/")
    ));

    assert!(is_gleam_path(
        &PathBuf::from("/some-prefix/one_two/a123.gleam"),
        &PathBuf::from("/some-prefix/")
    ));

    assert!(is_gleam_path(
        &PathBuf::from("/some-prefix/one_2/a123.gleam"),
        &PathBuf::from("/some-prefix/")
    ));
}

pub fn gleam_files(dir: &PathBuf) -> impl Iterator<Item = PathBuf> + '_ {
    walkdir::WalkDir::new(dir.clone())
        .follow_links(true)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .map(|d| d.path().to_path_buf())
        .filter(move |d| is_gleam_path(d, &dir))
}

pub fn create_tar_archive(outputs: Vec<OutputFile>) -> Result<Vec<u8>, Error> {
    let encoder = GzEncoder::new(vec![], Compression::default());
    let mut builder = tar::Builder::new(encoder);

    for file in outputs {
        let mut header = tar::Header::new_gnu();
        header.set_path(&file.path).map_err(|e| Error::Tar {
            path: file.path.clone(),
            err: e.to_string(),
        })?;
        header.set_size(file.text.as_bytes().len() as u64);
        header.set_cksum();
        builder
            .append(&header, file.text.as_bytes())
            .map_err(|e| Error::Tar {
                path: file.path.clone(),
                err: e.to_string(),
            })?;
    }

    builder
        .into_inner()
        .map_err(|e| Error::TarFinish(e.to_string()))?
        .finish()
        .map_err(|e| Error::Gzip(e.to_string()))
}

pub fn mkdir(path: impl AsRef<Path>) -> Result<(), Error> {
    std::fs::create_dir_all(&path).map_err(|err| Error::FileIO {
        kind: FileKind::Directory,
        path: PathBuf::from(path.as_ref()),
        action: FileIOAction::Create,
        err: Some(err.to_string()),
    })
}

pub fn read_dir(path: impl AsRef<Path>) -> Result<std::fs::ReadDir, Error> {
    std::fs::read_dir(&path).map_err(|e| Error::FileIO {
        action: FileIOAction::Read,
        kind: FileKind::Directory,
        path: PathBuf::from(path.as_ref()),
        err: Some(e.to_string()),
    })
}

pub fn read(path: impl AsRef<Path>) -> Result<String, Error> {
    std::fs::read_to_string(&path).map_err(|err| Error::FileIO {
        action: FileIOAction::Read,
        kind: FileKind::File,
        path: PathBuf::from(path.as_ref()),
        err: Some(err.to_string()),
    })
}
