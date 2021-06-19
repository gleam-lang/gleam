use crate::error::{Error, FileIoAction, FileKind, Result};
use std::{
    fmt::Debug,
    path::{Path, PathBuf},
};

pub trait Utf8Writer: std::fmt::Write {
    /// A wrapper around `fmt::Write` that has Gleam's error handling.
    fn str_write(&mut self, str: &str) -> Result<()> {
        let res = self.write_str(str);
        self.wrap_result(res)
    }

    fn wrap_result<T, E: std::error::Error>(&self, result: Result<T, E>) -> Result<()> {
        self.convert_err(result.map(|_| ()))
    }

    fn convert_err<T, E: std::error::Error>(&self, result: Result<T, E>) -> Result<T>;
}

impl Utf8Writer for String {
    fn convert_err<T, E: std::error::Error>(&self, result: Result<T, E>) -> Result<T> {
        result.map_err(|error| Error::FileIo {
            action: FileIoAction::WriteTo,
            kind: FileKind::File,
            path: PathBuf::from("<in memory>"),
            err: Some(error.to_string()),
        })
    }
}

pub trait Writer: std::io::Write + Utf8Writer {
    /// A wrapper around `io::Write` that has Gleam's error handling.
    fn write(&mut self, bytes: &[u8]) -> Result<(), Error> {
        let res = std::io::Write::write(self, bytes);
        self.wrap_result(res)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct OutputFile {
    pub text: String,
    pub path: PathBuf,
}

/// A trait used to read files.
/// Typically we use an implementation that reads from the file system,
/// but in tests and in other places other implementations may be used.
pub trait FileSystemReader {
    fn gleam_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>>;

    fn read<P>(&self, path: P) -> Result<String, Error>
    where
        P: AsRef<Path> + Debug;
}

pub trait FileSystemIO: FileSystemWriter + FileSystemReader {}

/// A trait used to write files.
/// Typically we use an implementation that writes to the file system,
/// but in tests and in other places other implementations may be used.
pub trait FileSystemWriter {
    fn open<'a>(&self, path: &'a Path) -> Result<WrappedWriter<'a>, Error>;
}

// TODO: Remove this when the Rust compiler stops incorrectly suggesting this
// could be derived. It can't because Write doesn't implement Debug
#[allow(missing_debug_implementations)]
/// A wrapper around a Write implementing object that has Gleam's error handling.
pub struct WrappedWriter<'a> {
    path: &'a Path,
    inner: Box<dyn std::io::Write>,
}

impl Writer for WrappedWriter<'_> {}

impl Utf8Writer for WrappedWriter<'_> {
    fn convert_err<T, E: std::error::Error>(&self, result: Result<T, E>) -> Result<T> {
        result.map_err(|error| Error::FileIo {
            action: FileIoAction::WriteTo,
            kind: FileKind::File,
            path: self.path.to_path_buf(),
            err: Some(error.to_string()),
        })
    }
}

impl<'a> WrappedWriter<'a> {
    pub fn new(path: &'a Path, inner: Box<dyn std::io::Write>) -> Self {
        Self { path, inner }
    }

    pub fn write(&'a mut self, bytes: &[u8]) -> Result<(), Error> {
        let result = self.inner.write(bytes);
        self.wrap_result(result)
    }
}

impl<'a> std::io::Write for WrappedWriter<'a> {
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

#[cfg(test)]
pub mod test {
    use super::*;
    use std::{
        cell::RefCell,
        io::Write,
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

    impl FileSystemReader for FilesChannel {
        fn gleam_files(&self, _dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
            unimplemented!()
        }

        fn read<P>(&self, _path: P) -> Result<String, Error>
        where
            P: AsRef<Path> + Debug,
        {
            unimplemented!()
        }
    }

    impl FileSystemIO for FilesChannel {}

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
                .map(RefCell::into_inner)
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
        fn convert_err<T, E: std::error::Error>(&self, result: Result<T, E>) -> Result<T> {
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
