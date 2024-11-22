use super::*;
use std::ops::Deref;
use std::path::Path;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
    time::Duration,
};

use camino::{Utf8Path, Utf8PathBuf};

/// An in memory sharable collection of pretend files that can be used in place
/// of a real file system. It is a shared reference to a set of buffer than can
/// be cheaply cloned, all resulting copies pointing to the same internal
/// buffers.
///
/// Useful in tests and in environments like the browser where there is no file
/// system.
///
/// Not thread safe. The compiler is single threaded, so that's OK.
///
/// Only supports absolute paths.
///
#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct InMemoryFileSystem {
    files: Rc<RefCell<HashMap<Utf8PathBuf, InMemoryFile>>>,
}

impl InMemoryFileSystem {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn reset(&self) {
        self.files.deref().borrow_mut().clear();
    }

    /// Returns the contents of each file, excluding directories.
    ///
    /// # Panics
    ///
    /// Panics if this is not the only reference to the underlying files.
    ///
    pub fn into_contents(self) -> HashMap<Utf8PathBuf, Content> {
        Rc::try_unwrap(self.files)
            .expect("InMemoryFileSystem::into_files called on a clone")
            .into_inner()
            .into_iter()
            .filter_map(|(path, file)| file.into_content().map(|content| (path, content)))
            .collect()
    }

    pub fn paths(&self) -> Vec<Utf8PathBuf> {
        self.files.borrow().keys().cloned().collect()
    }

    #[cfg(test)]
    /// Set the modification time of a file.
    ///
    /// Panics if the file does not exist.
    ///
    pub fn set_modification_time(&self, path: &Utf8Path, time: SystemTime) {
        self.files
            .deref()
            .borrow_mut()
            .get_mut(path)
            .unwrap()
            .modification_time = time;
    }

    pub fn try_set_modification_time(
        &self,
        path: &Utf8Path,
        time: SystemTime,
    ) -> Result<(), Error> {
        self.files
            .deref()
            .borrow_mut()
            .get_mut(path)
            .ok_or_else(|| Error::FileIo {
                kind: FileKind::File,
                action: FileIoAction::Open,
                path: path.to_path_buf(),
                err: None,
            })?
            .modification_time = time;
        Ok(())
    }

    /// Search for files inside a directory and its subdirectories using a
    /// certain path filter.
    pub fn search_files_recursively(
        &self,
        dir: impl AsRef<Path>,
        path_filter: impl Fn(&Utf8PathBuf) -> bool,
    ) -> Vec<Utf8PathBuf> {
        self.files
            .deref()
            .borrow()
            .iter()
            .filter(|(_, file)| !file.is_directory())
            .map(|(file_path, _)| file_path.to_path_buf())
            .filter(|file_path| file_path.starts_with(&dir))
            .filter(path_filter)
            .collect()
    }
}

impl FileSystemWriter for InMemoryFileSystem {
    fn delete_directory(&self, path: &Utf8Path) -> Result<(), Error> {
        let mut files = self.files.deref().borrow_mut();

        if files.get(path).is_some_and(|f| !f.is_directory()) {
            return Err(Error::FileIo {
                kind: FileKind::Directory,
                action: FileIoAction::Delete,
                path: path.to_path_buf(),
                err: None,
            });
        }

        let _ = files.remove(path);

        // Remove any files in the directory
        while let Some(file) = files.keys().find(|file| file.starts_with(path)) {
            let file = file.clone();
            let _ = files.remove(&file);
        }

        Ok(())
    }

    fn copy(&self, from: &Utf8Path, to: &Utf8Path) -> Result<(), Error> {
        self.write_bytes(to, &self.read_bytes(from)?)
    }

    fn copy_dir(&self, _: &Utf8Path, _: &Utf8Path) -> Result<(), Error> {
        panic!("unimplemented") // TODO
    }

    fn mkdir(&self, path: &Utf8Path) -> Result<(), Error> {
        // Traverse ancestors from parent to root
        // Create each missing ancestor
        for ancestor in path.ancestors() {
            if ancestor == "" {
                continue;
            }
            // Ensure we don't overwrite an existing file.
            // We can ignore existing directories though.
            let mut files = self.files.deref().borrow_mut();
            if files.get(ancestor).is_some_and(|f| !f.is_directory()) {
                return Err(Error::FileIo {
                    kind: FileKind::Directory,
                    action: FileIoAction::Create,
                    path: ancestor.to_path_buf(),
                    err: None,
                });
            }
            let dir = InMemoryFile::directory();
            _ = files.insert(ancestor.to_path_buf(), dir);
        }

        Ok(())
    }

    fn hardlink(&self, _: &Utf8Path, _: &Utf8Path) -> Result<(), Error> {
        panic!("unimplemented") // TODO
    }

    fn symlink_dir(&self, _: &Utf8Path, _: &Utf8Path) -> Result<(), Error> {
        panic!("unimplemented") // TODO
    }

    fn delete_file(&self, path: &Utf8Path) -> Result<(), Error> {
        let mut files = self.files.deref().borrow_mut();
        if files.get(path).is_some_and(|f| f.is_directory()) {
            return Err(Error::FileIo {
                kind: FileKind::File,
                action: FileIoAction::Delete,
                path: path.to_path_buf(),
                err: None,
            });
        }
        let _ = files.remove(path);
        Ok(())
    }

    fn write(&self, path: &Utf8Path, content: &str) -> Result<(), Error> {
        self.write_bytes(path, content.as_bytes())
    }

    fn write_bytes(&self, path: &Utf8Path, content: &[u8]) -> Result<(), Error> {
        // Ensure directories exist
        if let Some(parent) = path.parent() {
            self.mkdir(parent)?;
        }

        let mut file = InMemoryFile::default();
        _ = io::Write::write(&mut file, content).expect("channel buffer write");
        _ = self
            .files
            .deref()
            .borrow_mut()
            .insert(path.to_path_buf(), file);
        Ok(())
    }

    fn exists(&self, path: &Utf8Path) -> bool {
        self.files.deref().borrow().contains_key(path)
    }
}

impl FileSystemReader for InMemoryFileSystem {
    fn canonicalise(&self, path: &Utf8Path) -> Result<Utf8PathBuf, Error> {
        Ok(path.to_path_buf())
    }

    fn gleam_source_files(&self, dir: &Utf8Path) -> Vec<Utf8PathBuf> {
        self.search_files_recursively(dir, |file_path| file_path.extension() == Some("gleam"))
    }

    fn gleam_cache_files(&self, dir: &Utf8Path) -> Vec<Utf8PathBuf> {
        self.search_files_recursively(dir, |file_path| file_path.extension() == Some("cache"))
    }

    fn gleam_source_and_native_files(&self, dir: &Utf8Path) -> Vec<Utf8PathBuf> {
        self.search_files_recursively(dir, |file_path| {
            matches!(
                file_path.extension(),
                Some("gleam" | "mjs" | "js" | "ts" | "hrl" | "erl" | "ex")
            )
        })
    }

    fn read(&self, path: &Utf8Path) -> Result<String, Error> {
        let path = path.to_path_buf();
        let files = self.files.deref().borrow();
        let buffer = files
            .get(&path)
            .and_then(|file| file.node.as_file_buffer())
            .ok_or_else(|| Error::FileIo {
                kind: FileKind::File,
                action: FileIoAction::Open,
                path: path.clone(),
                err: None,
            })?;
        let bytes = buffer.borrow();
        let unicode = String::from_utf8(bytes.clone()).map_err(|err| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Read,
            path: path.clone(),
            err: Some(err.to_string()),
        })?;
        Ok(unicode)
    }

    fn read_bytes(&self, path: &Utf8Path) -> Result<Vec<u8>, Error> {
        let path = path.to_path_buf();
        let files = self.files.deref().borrow();
        let buffer = files
            .get(&path)
            .and_then(|file| file.node.as_file_buffer())
            .ok_or_else(|| Error::FileIo {
                kind: FileKind::File,
                action: FileIoAction::Open,
                path: path.clone(),
                err: None,
            })?;
        let bytes = buffer.borrow().clone();
        Ok(bytes)
    }

    fn is_file(&self, path: &Utf8Path) -> bool {
        self.files
            .deref()
            .borrow()
            .get(path)
            .is_some_and(|file| !file.is_directory())
    }

    fn is_directory(&self, path: &Utf8Path) -> bool {
        self.files
            .deref()
            .borrow()
            .get(path)
            .is_some_and(|file| file.is_directory())
    }

    fn reader(&self, _path: &Utf8Path) -> Result<WrappedReader, Error> {
        // TODO
        unreachable!("Memory reader unimplemented")
    }

    fn read_dir(&self, path: &Utf8Path) -> Result<ReadDir> {
        let read_dir = ReadDir::from_iter(
            self.files
                .deref()
                .borrow()
                .iter()
                .map(|(file_path, _)| file_path.to_path_buf())
                .filter(|file_path| file_path.parent().is_some_and(|parent| path == parent))
                .map(DirEntry::from_pathbuf)
                .map(Ok),
        );

        Ok(read_dir)
    }

    fn modification_time(&self, path: &Utf8Path) -> Result<SystemTime, Error> {
        let files = self.files.deref().borrow();
        let file = files.get(path).ok_or_else(|| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::ReadMetadata,
            path: path.to_path_buf(),
            err: None,
        })?;
        Ok(file.modification_time)
    }
}

/// The representation of a file or directory in the in-memory filesystem.
///
/// Stores a file's buffer of contents.
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InMemoryFileNode {
    File(Rc<RefCell<Vec<u8>>>),
    Directory,
}

impl InMemoryFileNode {
    /// Returns this file's file buffer if this isn't a directory.
    fn as_file_buffer(&self) -> Option<&Rc<RefCell<Vec<u8>>>> {
        match self {
            Self::File(buffer) => Some(buffer),
            Self::Directory => None,
        }
    }

    /// Returns this file's file buffer if this isn't a directory.
    fn into_file_buffer(self) -> Option<Rc<RefCell<Vec<u8>>>> {
        match self {
            Self::File(buffer) => Some(buffer),
            Self::Directory => None,
        }
    }
}

/// An in memory sharable that can be used in place of a real file. It is a
/// shared reference to a buffer than can be cheaply cloned, all resulting copies
/// pointing to the same internal buffer.
///
/// Useful in tests and in environments like the browser where there is no file
/// system.
///
/// This struct holds common properties of different types of filesystem nodes
/// (files and directories). The `node` field contains the file's content
/// buffer, if this is not a directory.
///
/// Not thread safe. The compiler is single threaded, so that's OK.
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InMemoryFile {
    node: InMemoryFileNode,
    modification_time: SystemTime,
}

impl InMemoryFile {
    /// Creates a directory.
    pub fn directory() -> Self {
        Self {
            node: InMemoryFileNode::Directory,
            ..Default::default()
        }
    }

    /// Checks whether this is a directory's entry.
    pub fn is_directory(&self) -> bool {
        matches!(self.node, InMemoryFileNode::Directory)
    }

    /// Returns this file's contents if this is not a directory.
    ///
    /// # Panics
    ///
    /// Panics if this is not the only reference to the underlying files.
    ///
    pub fn into_content(self) -> Option<Content> {
        let buffer = self.node.into_file_buffer()?;
        let contents = Rc::try_unwrap(buffer)
            .expect("InMemoryFile::into_content called with multiple references")
            .into_inner();
        match String::from_utf8(contents) {
            Ok(s) => Some(Content::Text(s)),
            Err(e) => Some(Content::Binary(e.into_bytes())),
        }
    }
}

impl Default for InMemoryFile {
    fn default() -> Self {
        Self {
            node: InMemoryFileNode::File(Default::default()),
            // We use a fixed time here so that the tests are deterministic. In
            // future we may want to inject this in some fashion.
            modification_time: SystemTime::UNIX_EPOCH + Duration::from_secs(663112800),
        }
    }
}

impl io::Write for InMemoryFile {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let Some(buffer) = self.node.as_file_buffer() else {
            // Not a file
            return Err(io::Error::from(io::ErrorKind::NotFound));
        };
        let mut reference = (*buffer).borrow_mut();
        reference.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        let Some(buffer) = self.node.as_file_buffer() else {
            // Not a file
            return Err(io::Error::from(io::ErrorKind::NotFound));
        };
        let mut reference = (*buffer).borrow_mut();
        reference.flush()
    }
}

impl CommandExecutor for InMemoryFileSystem {
    fn exec(
        &self,
        _program: &str,
        _args: &[String],
        _env: &[(&str, String)],
        _cwd: Option<&Utf8Path>,
        _stdio: Stdio,
    ) -> Result<i32, Error> {
        Ok(0) // Always succeed.
    }
}

impl BeamCompiler for InMemoryFileSystem {
    fn compile_beam(
        &self,
        _out: &Utf8Path,
        _lib: &Utf8Path,
        _modules: &HashSet<Utf8PathBuf>,
        _stdio: Stdio,
    ) -> Result<(), Error> {
        Ok(()) // Always succeed.
    }
}
