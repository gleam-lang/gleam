use crate::error::{Error, FileIoAction, FileKind, GleamExpect};
use flate2::{write::GzEncoder, Compression};
use std::{
    ffi::OsStr,
    fmt::Debug,
    fs::File,
    io::{BufRead, BufReader, Write},
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
pub trait FileSystemWriter: Debug {
    fn open<'a>(&self, path: &'a Path) -> Result<WrappedWriter<'a>, Error>;
}

/// A `FileWriter` implementation that writes to the file system.
#[derive(Debug, Clone, Copy)]
pub struct FileSystemAccessor;

impl FileSystemAccessor {
    pub fn new() -> Self {
        Self
    }
}

impl FileSystemWriter for FileSystemAccessor {
    fn open<'a>(&self, path: &'a Path) -> Result<WrappedWriter<'a>, Error> {
        tracing::trace!("Writing file {:?}", path);

        let dir_path = path.parent().ok_or_else(|| Error::FileIo {
            action: FileIoAction::FindParent,
            kind: FileKind::Directory,
            path: path.to_path_buf(),
            err: None,
        })?;

        std::fs::create_dir_all(dir_path).map_err(|e| Error::FileIo {
            action: FileIoAction::Create,
            kind: FileKind::Directory,
            path: dir_path.to_path_buf(),
            err: Some(e.to_string()),
        })?;

        let file = File::create(&path).map_err(|e| Error::FileIo {
            action: FileIoAction::Create,
            kind: FileKind::File,
            path: path.to_path_buf(),
            err: Some(e.to_string()),
        })?;

        Ok(WrappedWriter::new(path, Box::new(file)))
    }
}

pub trait Utf8Writer: std::fmt::Write {
    /// A wrapper around `fmt::Write` that has Gleam's error handling.
    fn str_write(&mut self, str: &str) -> Result<(), Error> {
        let res = self.write_str(str);
        self.wrap_result(res)
    }

    fn wrap_result<T, E: std::error::Error>(&self, result: Result<T, E>) -> crate::Result<()> {
        self.convert_err(result.map(|_| ()))
    }

    fn convert_err<T, E: std::error::Error>(&self, result: Result<T, E>) -> crate::Result<T>;
}

impl Utf8Writer for String {
    fn convert_err<T, E: std::error::Error>(&self, result: Result<T, E>) -> crate::Result<T> {
        result.map_err(|error| Error::FileIo {
            action: FileIoAction::WriteTo,
            kind: FileKind::File,
            path: PathBuf::from("<in memory>"),
            err: Some(error.to_string()),
        })
    }
}

pub trait Writer: Write + Utf8Writer {
    /// A wrapper around `io::Write` that has Gleam's error handling.
    fn write(&mut self, bytes: &[u8]) -> Result<(), Error> {
        let res = std::io::Write::write(self, bytes);
        self.wrap_result(res)
    }
}

// TODO: Remove this when the Rust compiler stops incorrectly suggesting this
// could be derived. It can't because Write doesn't implement Debug
#[allow(missing_debug_implementations)]
/// A wrapper around a Write implementing object that has Gleam's error handling.
pub struct WrappedWriter<'a> {
    path: &'a Path,
    inner: Box<dyn Write>,
}

impl Writer for WrappedWriter<'_> {}

impl Utf8Writer for WrappedWriter<'_> {
    fn convert_err<T, E: std::error::Error>(&self, result: Result<T, E>) -> crate::Result<T> {
        result.map_err(|error| Error::FileIo {
            action: FileIoAction::WriteTo,
            kind: FileKind::File,
            path: self.path.to_path_buf(),
            err: Some(error.to_string()),
        })
    }
}

impl<'a> WrappedWriter<'a> {
    pub fn new(path: &'a Path, inner: Box<dyn Write>) -> Self {
        Self { path, inner }
    }

    pub fn write(&'a mut self, bytes: &[u8]) -> Result<(), Error> {
        let result = self.inner.write(bytes);
        self.wrap_result(result)
    }
}

impl<'a> Write for WrappedWriter<'a> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.inner.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

impl<'a> std::fmt::Write for WrappedWriter<'a> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.inner
            .write(s.as_bytes())
            .map(|_| ())
            .map_err(|_| std::fmt::Error)
    }
}

pub fn delete_dir(dir: &PathBuf) -> Result<(), Error> {
    tracing::trace!("Deleting directory {:?}", dir);
    if dir.exists() {
        std::fs::remove_dir_all(&dir).map_err(|e| Error::FileIo {
            action: FileIoAction::Delete,
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

    let dir_path = path.parent().ok_or_else(|| Error::FileIo {
        action: FileIoAction::FindParent,
        kind: FileKind::Directory,
        path: path.clone(),
        err: None,
    })?;

    std::fs::create_dir_all(dir_path).map_err(|e| Error::FileIo {
        action: FileIoAction::Create,
        kind: FileKind::Directory,
        path: dir_path.to_path_buf(),
        err: Some(e.to_string()),
    })?;

    let mut f = File::create(&path).map_err(|e| Error::FileIo {
        action: FileIoAction::Create,
        kind: FileKind::File,
        path: path.clone(),
        err: Some(e.to_string()),
    })?;

    f.write_all(text.as_bytes()).map_err(|e| Error::FileIo {
        action: FileIoAction::WriteTo,
        kind: FileKind::File,
        path: path.clone(),
        err: Some(e.to_string()),
    })?;
    Ok(())
}

fn is_gleam_path(path: &Path, dir: impl AsRef<Path>) -> bool {
    use regex::Regex;
    lazy_static! {
        static ref RE: Regex = Regex::new(&format!(
            "^({module}{slash})*{module}\\.gleam$",
            module = "[a-z][_a-z0-9]*",
            slash = "(/|\\\\)",
        ))
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

    std::fs::create_dir_all(&path).map_err(|err| Error::FileIo {
        kind: FileKind::Directory,
        path: PathBuf::from(path.as_ref()),
        action: FileIoAction::Create,
        err: Some(err.to_string()),
    })
}

pub fn read_dir(path: impl AsRef<Path> + Debug) -> Result<std::fs::ReadDir, Error> {
    tracing::trace!("Reading directory {:?}", path);

    std::fs::read_dir(&path).map_err(|e| Error::FileIo {
        action: FileIoAction::Read,
        kind: FileKind::Directory,
        path: PathBuf::from(path.as_ref()),
        err: Some(e.to_string()),
    })
}

pub fn gleam_modules_metadata_paths(
    path: impl AsRef<Path> + Debug,
) -> Result<impl Iterator<Item = PathBuf>, Error> {
    Ok(read_dir(path)?
        .into_iter()
        .filter_map(Result::ok)
        .map(|f| f.path())
        .filter(|p| p.extension().and_then(OsStr::to_str) == Some("gleam_module")))
}

pub fn read(path: impl AsRef<Path> + Debug) -> Result<String, Error> {
    tracing::trace!("Reading file {:?}", path);

    std::fs::read_to_string(&path).map_err(|err| Error::FileIo {
        action: FileIoAction::Read,
        kind: FileKind::File,
        path: PathBuf::from(path.as_ref()),
        err: Some(err.to_string()),
    })
}

pub fn buffered_reader<P: AsRef<Path> + Debug>(path: P) -> Result<impl BufRead, Error> {
    tracing::trace!("Opening {:?} for reading", path);
    let reader = File::open(&path).map_err(|err| Error::FileIo {
        action: FileIoAction::Open,
        kind: FileKind::File,
        path: PathBuf::from(path.as_ref()),
        err: Some(err.to_string()),
    })?;
    Ok(BufReader::new(reader))
}

pub fn copy(path: impl AsRef<Path> + Debug, to: impl AsRef<Path> + Debug) -> Result<(), Error> {
    tracing::trace!("Copying file {:?} to {:?}", path, to);

    // TODO: include the destination in the error message
    std::fs::copy(&path, &to)
        .map_err(|err| Error::FileIo {
            action: FileIoAction::Copy,
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
        .map_err(|err| Error::FileIo {
            action: FileIoAction::Copy,
            kind: FileKind::Directory,
            path: PathBuf::from(path.as_ref()),
            err: Some(err.to_string()),
        })
        .map(|_| ())
}

#[cfg(test)]
pub mod test {
    use super::*;
    use std::{
        cell::RefCell,
        rc::Rc,
        sync::mpsc::{self, Receiver, Sender},
    };

    #[derive(Debug, Clone)]
    pub struct FilesChannel(Sender<(PathBuf, InMemoryFile)>);

    impl FilesChannel {
        pub fn new() -> (Self, Receiver<(PathBuf, InMemoryFile)>) {
            let (sender, receiver) = mpsc::channel();
            (Self(sender), receiver)
        }

        pub fn recv_utf8_files(
            receiver: &Receiver<(PathBuf, InMemoryFile)>,
        ) -> Result<Vec<OutputFile>, ()> {
            receiver
                .try_iter()
                .map(|(path, file)| {
                    Ok(OutputFile {
                        path,
                        text: String::from_utf8(file.into_contents()?).map_err(|_| ())?,
                    })
                })
                .collect()
        }
    }

    impl FileSystemWriter for FilesChannel {
        fn open<'a>(&self, path: &'a Path) -> Result<WrappedWriter<'a>, Error> {
            let file = InMemoryFile::new();
            let _ = self.0.send((path.to_path_buf(), file.clone()));
            Ok(WrappedWriter::new(path, Box::new(file)))
        }
    }

    #[derive(Debug, Default, Clone)]
    pub struct InMemoryFile {
        contents: Rc<RefCell<Vec<u8>>>,
    }

    impl InMemoryFile {
        pub fn new() -> Self {
            Default::default()
        }

        pub fn into_contents(self) -> Result<Vec<u8>, ()> {
            Rc::try_unwrap(self.contents)
                .map_err(|_| ())
                .map(|cell| cell.into_inner())
        }
    }

    impl Write for InMemoryFile {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.contents.borrow_mut().write(buf)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.contents.borrow_mut().flush()
        }
    }

    impl std::fmt::Write for InMemoryFile {
        fn write_str(&mut self, s: &str) -> std::fmt::Result {
            self.contents
                .borrow_mut()
                .write(s.as_bytes())
                .map(|_| ())
                .map_err(|_| std::fmt::Error)
        }
    }

    impl Utf8Writer for InMemoryFile {
        fn convert_err<T, E: std::error::Error>(&self, result: Result<T, E>) -> crate::Result<T> {
            result.map_err(|error| Error::FileIo {
                action: FileIoAction::WriteTo,
                kind: FileKind::File,
                path: PathBuf::from("<in memory test file>"),
                err: Some(error.to_string()),
            })
        }
    }

    impl Writer for InMemoryFile {}
}
