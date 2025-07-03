pub mod memory;

use crate::error::{Error, FileIoAction, FileKind, Result};
use async_trait::async_trait;
use debug_ignore::DebugIgnore;
use flate2::read::GzDecoder;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
    io,
    iter::Extend,
    time::SystemTime,
    vec::IntoIter,
};
use tar::{Archive, Entry};

use camino::{Utf8Path, Utf8PathBuf};

/// Takes in a source path and a target path and determines a relative path
/// from source -> target.
/// If given a relative target path, no calculation occurs.
/// # Panics
/// The provided source path should be absolute, otherwise will panic.
pub fn make_relative(source_path: &Utf8Path, target_path: &Utf8Path) -> Utf8PathBuf {
    assert!(source_path.is_absolute());
    // Input target will always be canonicalised whereas source will not
    // This causes problems with diffing on windows since canonicalised paths have a special root
    // As such we are attempting to strip the target path
    // Based on https://github.com/rust-lang/rust/issues/42869#issuecomment-1712317081
    #[cfg(target_family = "windows")]
    let binding = target_path.to_string();
    #[cfg(target_family = "windows")]
    let target_path = Utf8Path::new(binding.trim_start_matches(r"\\?\"));

    match target_path.is_absolute() {
        true => pathdiff::diff_utf8_paths(target_path, source_path)
            .expect("Should not fail on two absolute paths"),

        false => target_path.into(),
    }
}

pub trait Reader: io::Read {
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

pub trait Writer: io::Write + Utf8Writer {
    /// A wrapper around `io::Write` that has Gleam's error handling.
    fn write(&mut self, bytes: &[u8]) -> Result<(), Error> {
        io::Write::write(self, bytes)
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

/// Structure holding state to walk across a directory's descendant files at
/// any level. Note that each descendant directory is only visited once
/// regardless of symlinks, avoiding infinite symlink loops.
#[derive(Debug, Clone)]
pub struct DirWalker {
    walk_queue: VecDeque<Utf8PathBuf>,
    dirs_walked: im::HashSet<Utf8PathBuf>,
}

impl DirWalker {
    /// Create a directory walker starting at the given path.
    pub fn new(dir: Utf8PathBuf) -> Self {
        Self {
            walk_queue: VecDeque::from([dir]),
            dirs_walked: im::HashSet::new(),
        }
    }

    /// Convert this walker to an iterator over file paths.
    ///
    /// This iterator calls [`Self::next_file`]. Errors are returned if certain
    /// directories cannot be read.
    pub fn into_file_iter(
        mut self,
        io: &impl FileSystemReader,
    ) -> impl Iterator<Item = Result<Utf8PathBuf>> + '_ {
        std::iter::from_fn(move || self.next_file(io).transpose())
    }

    /// Advance the directory walker to the next file. The returned path will
    /// be relative to the starting directory's path, even with symlinks
    /// (it is not canonicalised).
    pub fn next_file(&mut self, io: &impl FileSystemReader) -> Result<Option<Utf8PathBuf>> {
        while let Some(next_path) = self.walk_queue.pop_front() {
            let real_path = io.canonicalise(&next_path)?;

            if io.is_file(&real_path) {
                // Return the path relative to the starting directory, not the
                // canonicalised path (which we only use to check for already
                // visited directories).
                return Ok(Some(next_path));
            }

            // If it's not a directory then it contains no other files, so there's nothing to do.
            if !io.is_directory(&real_path) {
                continue;
            }

            // If we have already processed this directory then we don't need to do it again.
            // This could be due to symlinks.
            let already_seen = self.dirs_walked.insert(real_path.clone()).is_some();
            if already_seen {
                continue;
            }

            for entry in io.read_dir(&next_path)? {
                let Ok(entry) = entry else {
                    return Err(Error::FileIo {
                        kind: FileKind::Directory,
                        action: FileIoAction::Read,
                        path: next_path,
                        err: None,
                    });
                };

                self.walk_queue.push_back(entry.into_path())
            }
        }

        Ok(None)
    }
}

/// A trait used to read files.
/// Typically we use an implementation that reads from the file system,
/// but in tests and in other places other implementations may be used.
pub trait FileSystemReader {
    fn read_dir(&self, path: &Utf8Path) -> Result<ReadDir>;
    fn read(&self, path: &Utf8Path) -> Result<String, Error>;
    fn read_bytes(&self, path: &Utf8Path) -> Result<Vec<u8>, Error>;
    fn reader(&self, path: &Utf8Path) -> Result<WrappedReader, Error>;
    fn is_file(&self, path: &Utf8Path) -> bool;
    fn is_directory(&self, path: &Utf8Path) -> bool;
    fn modification_time(&self, path: &Utf8Path) -> Result<SystemTime, Error>;
    fn canonicalise(&self, path: &Utf8Path) -> Result<Utf8PathBuf, Error>;
}

/// Iterates over files with the given extension in a certain directory.
/// Symlinks are followed.
pub fn files_with_extension<'a>(
    io: &'a impl FileSystemReader,
    dir: &'a Utf8Path,
    extension: &'a str,
) -> impl Iterator<Item = Utf8PathBuf> + 'a {
    DirWalker::new(dir.to_path_buf())
        .into_file_iter(io)
        .filter_map(Result::ok)
        .filter(|path| path.extension() == Some(extension))
}

/// A trait used to run other programs.
pub trait CommandExecutor {
    fn exec(&self, command: Command) -> Result<i32, Error>;
}

/// A command one can run with a `CommandExecutor`
#[derive(Debug, Eq, PartialEq)]
pub struct Command {
    pub program: String,
    pub args: Vec<String>,
    pub env: Vec<(String, String)>,
    pub cwd: Option<Utf8PathBuf>,
    pub stdio: Stdio,
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

/// A trait used to compile Erlang and Elixir modules to BEAM bytecode.
pub trait BeamCompiler {
    fn compile_beam(
        &self,
        out: &Utf8Path,
        lib: &Utf8Path,
        modules: &HashSet<Utf8PathBuf>,
        stdio: Stdio,
    ) -> Result<Vec<String>, Error>;
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
    fn exists(&self, path: &Utf8Path) -> bool;
}

#[derive(Debug)]
/// A wrapper around a Read implementing object that has Gleam's error handling.
pub struct WrappedReader {
    path: Utf8PathBuf,
    inner: DebugIgnore<Box<dyn io::Read>>,
}

impl WrappedReader {
    pub fn new(path: &Utf8Path, inner: Box<dyn io::Read>) -> Self {
        Self {
            path: path.to_path_buf(),
            inner: DebugIgnore(inner),
        }
    }

    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
        self.inner.read(buffer)
    }
}

impl io::Read for WrappedReader {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
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

#[inline]
pub fn is_native_file_extension(extension: &str) -> bool {
    matches!(
        extension,
        "erl" | "hrl" | "ex" | "js" | "mjs" | "cjs" | "ts"
    )
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
