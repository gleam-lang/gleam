pub mod memory;

use crate::error::{Error, FileIoAction, FileKind, Result};
use async_trait::async_trait;
use debug_ignore::DebugIgnore;
use flate2::read::GzDecoder;
use std::{collections::HashMap, fmt::Debug, io, time::SystemTime, vec::IntoIter};
use tar::{Archive, Entry};

use camino::{Utf8Path, Utf8PathBuf};

/// Takes in a source path and a target path and determines a relative path
/// from source -> target.
/// If given a relative target path, no calculation occurs.
/// # Panics
/// The provided source path should be absolute, otherwise will panic.
pub fn make_relative(source_path: &Utf8Path, target_path: &Utf8Path) -> Utf8PathBuf {
    assert!(source_path.is_absolute());
    match target_path.is_absolute() {
        true => pathdiff::diff_utf8_paths(target_path, source_path)
            .expect("Should not fail on two absolute paths"),

        false => target_path.into(),
    }
}

pub trait Reader: std::io::Read {
    /// A wrapper around `std::io::Read` that has Gleam's error handling.
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
            path: Utf8PathBuf::from("<in memory>"),
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Content {
    Binary(Vec<u8>),
    Text(String),
}

impl Content {
    pub fn as_bytes(&self) -> &[u8] {
        match self {
            Content::Binary(data) => data,
            Content::Text(data) => data.as_bytes(),
        }
    }

    pub fn text(&self) -> Option<&str> {
        match self {
            Content::Binary(_) => None,
            Content::Text(s) => Some(s),
        }
    }
}

impl From<Vec<u8>> for Content {
    fn from(bytes: Vec<u8>) -> Self {
        Content::Binary(bytes)
    }
}

impl From<&[u8]> for Content {
    fn from(bytes: &[u8]) -> Self {
        Content::Binary(bytes.to_vec())
    }
}

impl From<String> for Content {
    fn from(text: String) -> Self {
        Content::Text(text)
    }
}

impl From<&str> for Content {
    fn from(text: &str) -> Self {
        Content::Text(text.to_string())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct OutputFile {
    pub content: Content,
    pub path: Utf8PathBuf,
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
        self.entries.extend(other);

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
    pub pathbuf: Utf8PathBuf,
}

impl DirEntry {
    pub fn from_path<P: AsRef<Utf8Path>>(path: P) -> DirEntry {
        DirEntry {
            pathbuf: path.as_ref().to_path_buf(),
        }
    }

    pub fn from_pathbuf(pathbuf: Utf8PathBuf) -> DirEntry {
        DirEntry { pathbuf }
    }

    pub fn as_path(&self) -> &Utf8Path {
        self.pathbuf.as_path()
    }

    pub fn into_path(self) -> Utf8PathBuf {
        self.pathbuf
    }
}

/// A trait used to read files.
/// Typically we use an implementation that reads from the file system,
/// but in tests and in other places other implementations may be used.
pub trait FileSystemReader {
    fn gleam_source_files(&self, dir: &Utf8Path) -> Vec<Utf8PathBuf>;
    fn gleam_cache_files(&self, dir: &Utf8Path) -> Vec<Utf8PathBuf>;
    fn read_dir(&self, path: &Utf8Path) -> Result<ReadDir>;
    fn read(&self, path: &Utf8Path) -> Result<String, Error>;
    fn read_bytes(&self, path: &Utf8Path) -> Result<Vec<u8>, Error>;
    fn reader(&self, path: &Utf8Path) -> Result<WrappedReader, Error>;
    fn is_file(&self, path: &Utf8Path) -> bool;
    fn is_directory(&self, path: &Utf8Path) -> bool;
    fn modification_time(&self, path: &Utf8Path) -> Result<SystemTime, Error>;
    fn canonicalise(&self, path: &Utf8Path) -> Result<Utf8PathBuf, Error>;
}

/// A trait used to run other programs.
pub trait CommandExecutor {
    fn exec(
        &self,
        program: &str,
        args: &[String],
        env: &[(&str, String)],
        cwd: Option<&Utf8Path>,
        stdio: Stdio,
    ) -> Result<i32, Error>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Stdio {
    Inherit,
    Null,
}

impl Stdio {
    pub fn get_process_stdio(&self) -> std::process::Stdio {
        match self {
            Stdio::Inherit => std::process::Stdio::inherit(),
            Stdio::Null => std::process::Stdio::null(),
        }
    }
}

/// A trait used to write files.
/// Typically we use an implementation that writes to the file system,
/// but in tests and in other places other implementations may be used.
pub trait FileSystemWriter {
    fn mkdir(&self, path: &Utf8Path) -> Result<(), Error>;
    fn write(&self, path: &Utf8Path, content: &str) -> Result<(), Error>;
    fn write_bytes(&self, path: &Utf8Path, content: &[u8]) -> Result<(), Error>;
    fn delete_directory(&self, path: &Utf8Path) -> Result<(), Error>;
    fn copy(&self, from: &Utf8Path, to: &Utf8Path) -> Result<(), Error>;
    fn copy_dir(&self, from: &Utf8Path, to: &Utf8Path) -> Result<(), Error>;
    fn hardlink(&self, from: &Utf8Path, to: &Utf8Path) -> Result<(), Error>;
    fn symlink_dir(&self, from: &Utf8Path, to: &Utf8Path) -> Result<(), Error>;
    fn delete_file(&self, path: &Utf8Path) -> Result<(), Error>;
}

#[derive(Debug)]
/// A wrapper around a Read implementing object that has Gleam's error handling.
pub struct WrappedReader {
    path: Utf8PathBuf,
    inner: DebugIgnore<Box<dyn std::io::Read>>,
}

impl WrappedReader {
    pub fn new(path: &Utf8Path, inner: Box<dyn std::io::Read>) -> Self {
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
        path: &Utf8Path,
        archive: Archive<GzDecoder<Entry<'_, WrappedReader>>>,
    ) -> io::Result<()>;

    fn unpack(
        &self,
        path: &Utf8Path,
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

pub fn ordered_map<S, K, V>(value: &HashMap<K, V>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    K: serde::Serialize + Ord,
    V: serde::Serialize,
{
    use serde::Serialize;
    let ordered: std::collections::BTreeMap<_, _> = value.iter().collect();
    ordered.serialize(serializer)
}
