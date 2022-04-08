pub mod memory;

use crate::error::{Error, FileIoAction, FileKind, Result};
use async_trait::async_trait;
use debug_ignore::DebugIgnore;
use flate2::read::GzDecoder;
use std::{
    fmt::Debug,
    io,
    path::{Path, PathBuf},
    vec::IntoIter,
};
use tar::{Archive, Entry};

pub trait Reader: std::io::Read {
    /// A wrapper around `std::io::Write` that has Gleam's error handling.
    fn read_bytes(&mut self, buffer: &mut [u8]) -> Result<usize> {
        self.read(buffer).map_err(|e| self.convert_err(e))
    }

    fn convert_err<E: std::error::Error>(&self, error: E) -> Error;
}

pub trait Utf8Writer: std::fmt::Write {
    /// A wrapper around `fmt::Write` that has Gleam's error handling.
    fn str_write(&mut self, str: &str) -> Result<()> {
        self.write_str(str).map_err(|e| self.convert_err(e))
    }

    fn convert_err<E: std::error::Error>(&self, err: E) -> Error;
}

impl Utf8Writer for String {
    fn convert_err<E: std::error::Error>(&self, error: E) -> Error {
        Error::FileIo {
            action: FileIoAction::WriteTo,
            kind: FileKind::File,
            path: PathBuf::from("<in memory>"),
            err: Some(error.to_string()),
        }
    }
}

pub trait Writer: std::io::Write + Utf8Writer {
    /// A wrapper around `io::Write` that has Gleam's error handling.
    fn write(&mut self, bytes: &[u8]) -> Result<(), Error> {
        std::io::Write::write(self, bytes)
            .map(|_| ())
            .map_err(|e| self.convert_err(e))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct OutputFile {
    pub text: String,
    pub path: PathBuf,
}

#[derive(Debug)]
pub struct ReadDir {
    entries: Vec<io::Result<DirEntry>>,
}

impl FromIterator<io::Result<DirEntry>> for ReadDir {
    fn from_iter<I: IntoIterator<Item = io::Result<DirEntry>>>(iter: I) -> Self {
        ReadDir {
            entries: iter.into_iter().collect(),
        }
    }
}

impl ReadDir {
    pub fn extend(mut self, other: ReadDir) -> Self {
        self.entries.extend(other.into_iter());

        ReadDir {
            entries: self.entries,
        }
    }
}

impl IntoIterator for ReadDir {
    type Item = io::Result<DirEntry>;
    type IntoIter = IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_iter()
    }
}

#[derive(Debug, Clone)]
pub struct DirEntry {
    pub pathbuf: PathBuf,
}

impl DirEntry {
    pub fn from_path<P: AsRef<Path>>(path: P) -> DirEntry {
        DirEntry {
            pathbuf: path.as_ref().to_path_buf(),
        }
    }

    pub fn from_pathbuf(pathbuf: PathBuf) -> DirEntry {
        DirEntry { pathbuf }
    }

    pub fn as_path(&self) -> &Path {
        self.pathbuf.as_path()
    }

    pub fn into_path(self) -> PathBuf {
        self.pathbuf
    }
}

/// A trait used to read files.
/// Typically we use an implementation that reads from the file system,
/// but in tests and in other places other implementations may be used.
pub trait FileSystemReader {
    fn gleam_source_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>>;
    fn gleam_metadata_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>>;
    fn read_dir(&self, path: &Path) -> Result<ReadDir>;
    fn read(&self, path: &Path) -> Result<String, Error>;
    fn reader(&self, path: &Path) -> Result<WrappedReader, Error>;
    fn is_file(&self, path: &Path) -> bool;
    fn is_directory(&self, path: &Path) -> bool;
}

pub trait FileSystemIO: FileSystemWriter + FileSystemReader {}

/// A trait used to run other programs.
pub trait CommandExecutor {
    fn exec(
        &self,
        program: &str,
        args: &[String],
        env: &[(&str, String)],
        cwd: Option<&Path>,
        // Whether to silence stdout
        quiet: bool,
    ) -> Result<i32, Error>;
}

/// A trait used to write files.
/// Typically we use an implementation that writes to the file system,
/// but in tests and in other places other implementations may be used.
pub trait FileSystemWriter {
    fn mkdir(&self, path: &Path) -> Result<(), Error>;
    fn writer(&self, path: &Path) -> Result<WrappedWriter, Error>;
    fn delete(&self, path: &Path) -> Result<(), Error>;
    fn copy(&self, from: &Path, to: &Path) -> Result<(), Error>;
    fn copy_dir(&self, from: &Path, to: &Path) -> Result<(), Error>;
    fn hardlink(&self, from: &Path, to: &Path) -> Result<(), Error>;
    fn symlink_dir(&self, from: &Path, to: &Path) -> Result<(), Error>;
    fn delete_file(&self, path: &Path) -> Result<(), Error>;
}

#[derive(Debug)]
/// A wrapper around a Read implementing object that has Gleam's error handling.
pub struct WrappedReader {
    path: PathBuf,
    inner: DebugIgnore<Box<dyn std::io::Read>>,
}

impl WrappedReader {
    pub fn new(path: &Path, inner: Box<dyn std::io::Read>) -> Self {
        Self {
            path: path.to_path_buf(),
            inner: DebugIgnore(inner),
        }
    }

    fn read(&mut self, buffer: &mut [u8]) -> std::io::Result<usize> {
        self.inner.read(buffer)
    }
}

impl std::io::Read for WrappedReader {
    fn read(&mut self, buffer: &mut [u8]) -> std::io::Result<usize> {
        self.read(buffer)
    }
}

impl Reader for WrappedReader {
    fn convert_err<E: std::error::Error>(&self, err: E) -> Error {
        Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Read,
            path: self.path.clone(),
            err: Some(err.to_string()),
        }
    }
}

#[derive(Debug)]
/// A wrapper around a Write implementing object that has Gleam's error handling.
pub struct WrappedWriter {
    pub path: PathBuf,
    pub inner: DebugIgnore<Box<dyn std::io::Write>>,
}

impl Writer for WrappedWriter {}

impl Utf8Writer for WrappedWriter {
    fn convert_err<E: std::error::Error>(&self, error: E) -> Error {
        Error::FileIo {
            action: FileIoAction::WriteTo,
            kind: FileKind::File,
            path: self.path.to_path_buf(),
            err: Some(error.to_string()),
        }
    }
}

impl WrappedWriter {
    pub fn new(path: &Path, inner: Box<dyn std::io::Write>) -> Self {
        Self {
            path: path.to_path_buf(),
            inner: DebugIgnore(inner),
        }
    }

    pub fn write(&mut self, bytes: &[u8]) -> Result<(), Error> {
        self.inner
            .write(bytes)
            .map(|_| ())
            .map_err(|e| self.convert_err(e))
    }
}

impl<'a> std::io::Write for WrappedWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.inner.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

impl<'a> std::fmt::Write for WrappedWriter {
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

    impl CommandExecutor for FilesChannel {
        fn exec(
            &self,
            _program: &str,
            _args: &[String],
            _env: &[(&str, String)],
            _cwd: Option<&Path>,
            _quiet: bool,
        ) -> Result<i32, Error> {
            Ok(0)
        }
    }

    impl FileSystemWriter for FilesChannel {
        fn writer<'a>(&self, path: &'a Path) -> Result<WrappedWriter, Error> {
            let file = InMemoryFile::new();
            let _ = self.0.send((path.to_path_buf(), file.clone()));
            Ok(WrappedWriter::new(path, Box::new(file)))
        }

        fn delete(&self, _path: &Path) -> Result<(), Error> {
            panic!("FilesChannel does not support deletion")
        }

        fn copy(&self, _from: &Path, _to: &Path) -> Result<(), Error> {
            panic!("FilesChannel does not support copy")
        }

        fn mkdir(&self, _path: &Path) -> Result<(), Error> {
            Ok(())
        }

        fn copy_dir(&self, _from: &Path, _to: &Path) -> Result<(), Error> {
            panic!("FilesChannel does not support copy_dir")
        }

        fn hardlink(&self, _: &Path, _: &Path) -> Result<(), Error> {
            panic!("FilesChannel does not support hardlink")
        }

        fn symlink_dir(&self, _: &Path, _: &Path) -> Result<(), Error> {
            panic!("FilesChannel does not support symlink")
        }

        fn delete_file(&self, _path: &Path) -> Result<(), Error> {
            panic!("FilesChannel does not support deletion")
        }
    }

    impl FileSystemReader for FilesChannel {
        fn gleam_source_files(&self, _dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
            unimplemented!()
        }

        fn gleam_metadata_files(&self, _dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
            unimplemented!()
        }

        fn read(&self, _path: &Path) -> Result<String, Error> {
            unimplemented!()
        }

        fn is_file(&self, _path: &Path) -> bool {
            unimplemented!()
        }

        fn reader(&self, _path: &Path) -> Result<WrappedReader, Error> {
            unimplemented!()
        }

        fn is_directory(&self, _path: &Path) -> bool {
            unimplemented!()
        }

        fn read_dir(&self, _path: &Path) -> Result<ReadDir> {
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
        fn convert_err<E: std::error::Error>(&self, error: E) -> Error {
            Error::FileIo {
                action: FileIoAction::WriteTo,
                kind: FileKind::File,
                path: PathBuf::from("<in memory test file>"),
                err: Some(error.to_string()),
            }
        }
    }

    impl Writer for InMemoryFile {}
}

#[async_trait]
pub trait HttpClient {
    async fn send(&self, request: http::Request<Vec<u8>>)
        -> Result<http::Response<Vec<u8>>, Error>;
}

pub trait TarUnpacker {
    // FIXME: The reader types are restrictive here. We should be more generic
    // than this.
    fn io_result_entries<'a>(
        &self,
        archive: &'a mut Archive<WrappedReader>,
    ) -> io::Result<tar::Entries<'a, WrappedReader>>;

    fn entries<'a>(
        &self,
        archive: &'a mut Archive<WrappedReader>,
    ) -> Result<tar::Entries<'a, WrappedReader>> {
        tracing::debug!("iterating through tar archive");
        self.io_result_entries(archive)
            .map_err(|e| Error::ExpandTar {
                error: e.to_string(),
            })
    }

    fn io_result_unpack(
        &self,
        path: &Path,
        archive: Archive<GzDecoder<Entry<'_, WrappedReader>>>,
    ) -> io::Result<()>;

    fn unpack(
        &self,
        path: &Path,
        archive: Archive<GzDecoder<Entry<'_, WrappedReader>>>,
    ) -> Result<()> {
        tracing::debug!(path = ?path, "unpacking tar archive");
        self.io_result_unpack(path, archive)
            .map_err(|e| Error::FileIo {
                action: FileIoAction::WriteTo,
                kind: FileKind::Directory,
                path: path.to_path_buf(),
                err: Some(e.to_string()),
            })
    }
}
