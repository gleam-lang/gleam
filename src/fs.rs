use crate::error::{Error, FileIOAction, FileKind, GleamExpect};
use flate2::{write::GzEncoder, Compression};
use std::{
    fmt::Debug,
    fs::File,
    io::Write,
    path::{Path, PathBuf},
};

#[derive(Debug, PartialEq, Clone)]
pub struct OutputFile {
    pub text: String,
    pub path: PathBuf,
}

/// A trait used to write files.
/// Typically we use an implementation that writes to the file system,
/// but in tests and in other places other implementations may be used.
pub trait FileWriter: Debug {
    fn open<'a>(&self, path: &'a Path) -> Result<Writer<'a>, Error>;
}

/// A FileWriter implementation that writes to the file system.
#[derive(Debug)]
pub struct FileSystemAccessor;

impl FileSystemAccessor {
    pub fn new() -> Self {
        Self
    }

    pub fn boxed() -> Box<Self> {
        Box::new(Self::new())
    }
}

impl FileWriter for FileSystemAccessor {
    fn open<'a>(&self, path: &'a Path) -> Result<Writer<'a>, Error> {
        tracing::trace!("Writing file {:?}", path);

        let dir_path = path.parent().ok_or_else(|| Error::FileIO {
            action: FileIOAction::FindParent,
            kind: FileKind::Directory,
            path: path.to_path_buf(),
            err: None,
        })?;

        std::fs::create_dir_all(dir_path).map_err(|e| Error::FileIO {
            action: FileIOAction::Create,
            kind: FileKind::Directory,
            path: dir_path.to_path_buf(),
            err: Some(e.to_string()),
        })?;

        let file = File::create(&path).map_err(|e| Error::FileIO {
            action: FileIOAction::Create,
            kind: FileKind::File,
            path: path.to_path_buf(),
            err: Some(e.to_string()),
        })?;

        Ok(Writer::new(path, Box::new(file)))
    }
}

// TODO: Remove this when the Rust compiler stops incorrectly suggesting this
// could be derived. It can't because Write doesn't implement Debug
#[allow(missing_debug_implementations)]
/// A wrapper around a Write implementing object that has Gleam's error handling.
pub struct Writer<'a> {
    path: &'a Path,
    inner: Box<dyn Write>,
}

impl<'a> Writer<'a> {
    pub fn new(path: &'a Path, inner: Box<dyn Write>) -> Self {
        Self { path, inner }
    }

    /// A wrapper around write that has Gleam's error handling.
    pub fn write(&'a mut self, bytes: &[u8]) -> Result<(), Error> {
        self.inner
            .write(bytes)
            .map_err(|error| Error::FileIO {
                action: FileIOAction::WriteTo,
                kind: FileKind::File,
                path: self.path.to_path_buf(),
                err: Some(error.to_string()),
            })
            .map(|_| ())
    }
}

pub fn delete_dir(dir: &PathBuf) -> Result<(), Error> {
    tracing::trace!("Deleting directory {:?}", dir);
    if dir.exists() {
        std::fs::remove_dir_all(&dir).map_err(|e| Error::FileIO {
            action: FileIOAction::Delete,
            kind: FileKind::Directory,
            path: dir.clone(),
            err: Some(e.to_string()),
        })?;
    } else {
        tracing::trace!("Did not exist for deletion: {:?}", dir);
    }
    Ok(())
}

// pub fn delete(file: &PathBuf) -> Result<(), Error> {
//     tracing::trace!("Deleting file {:?}", file);
//     if file.exists() {
//         std::fs::remove_file(&file).map_err(|e| Error::FileIO {
//             action: FileIOAction::Delete,
//             kind: FileKind::File,
//             path: file.clone(),
//             err: Some(e.to_string()),
//         })?;
//     } else {
//         tracing::trace!("Did not exist for deletion: {:?}", file);
//     }
//     Ok(())
// }

pub fn write_outputs(outputs: &[OutputFile]) -> Result<(), Error> {
    for file in outputs {
        write_output(file)?;
    }
    Ok(())
}

pub fn write_output(file: &OutputFile) -> Result<(), Error> {
    let OutputFile { path, text } = file;
    tracing::trace!("Writing file {:?}", path);

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

fn is_gleam_path(path: &PathBuf, dir: impl AsRef<Path>) -> bool {
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

pub fn gleam_files(dir: &Path) -> impl Iterator<Item = PathBuf> + '_ {
    walkdir::WalkDir::new(dir)
        .follow_links(true)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .map(|d| d.path().to_path_buf())
        .filter(move |d| is_gleam_path(d, dir))
}

pub fn gleam_files_excluding_gitignore(dir: &PathBuf) -> impl Iterator<Item = PathBuf> + '_ {
    ignore::WalkBuilder::new(&dir)
        .follow_links(true)
        .require_git(false)
        .build()
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().map(|t| t.is_file()).unwrap_or(false))
        .map(|d| d.into_path())
        .filter(move |d| is_gleam_path(d, dir))
}

pub fn create_tar_archive(outputs: Vec<OutputFile>) -> Result<Vec<u8>, Error> {
    tracing::trace!("Creating tarball archive");

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

pub fn mkdir(path: impl AsRef<Path> + Debug) -> Result<(), Error> {
    tracing::trace!("Creating directory {:?}", path);

    std::fs::create_dir_all(&path).map_err(|err| Error::FileIO {
        kind: FileKind::Directory,
        path: PathBuf::from(path.as_ref()),
        action: FileIOAction::Create,
        err: Some(err.to_string()),
    })
}

pub fn read_dir(path: impl AsRef<Path> + Debug) -> Result<std::fs::ReadDir, Error> {
    tracing::trace!("Reading directory {:?}", path);

    std::fs::read_dir(&path).map_err(|e| Error::FileIO {
        action: FileIOAction::Read,
        kind: FileKind::Directory,
        path: PathBuf::from(path.as_ref()),
        err: Some(e.to_string()),
    })
}

pub fn read(path: impl AsRef<Path> + Debug) -> Result<String, Error> {
    tracing::trace!("Reading file {:?}", path);

    std::fs::read_to_string(&path).map_err(|err| Error::FileIO {
        action: FileIOAction::Read,
        kind: FileKind::File,
        path: PathBuf::from(path.as_ref()),
        err: Some(err.to_string()),
    })
}

pub fn copy(path: impl AsRef<Path> + Debug, to: impl AsRef<Path> + Debug) -> Result<(), Error> {
    tracing::trace!("Copying file {:?} to {:?}", path, to);

    // TODO: include the destination in the error message
    std::fs::copy(&path, &to)
        .map_err(|err| Error::FileIO {
            action: FileIOAction::Copy,
            kind: FileKind::File,
            path: PathBuf::from(path.as_ref()),
            err: Some(err.to_string()),
        })
        .map(|_| ())
}

pub fn copy_dir(path: impl AsRef<Path> + Debug, to: impl AsRef<Path> + Debug) -> Result<(), Error> {
    tracing::trace!("Copying directory {:?} to {:?}", path, to);

    // TODO: include the destination in the error message
    fs_extra::dir::copy(&path, &to, &fs_extra::dir::CopyOptions::new())
        .map_err(|err| Error::FileIO {
            action: FileIOAction::Copy,
            kind: FileKind::Directory,
            path: PathBuf::from(path.as_ref()),
            err: Some(err.to_string()),
        })
        .map(|_| ())
}
