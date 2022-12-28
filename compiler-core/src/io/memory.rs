use lazy_static::__Deref;

use super::*;
use std::{cell::RefCell, collections::HashMap, ffi::OsStr, rc::Rc, time::Duration};

// An in memory sharable collection of pretend files that can be used in place
// of a real file system. It is a shared reference to a set of buffer than can
// be cheaply cloned, all resulting copies pointing to the same internal
// buffers.
//
// Useful in tests and in environments like the browser where there is no file
// system.
//
// Not thread safe. The compiler is single threaded, so that's OK.
//
// Only supports absolute paths. For now. In future we could have a explicit
// current directory, or say that the current directory is always the root.
//
#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct InMemoryFileSystem {
    files: Rc<RefCell<HashMap<PathBuf, InMemoryFile>>>,
}

impl InMemoryFileSystem {
    pub fn new() -> Self {
        Self::default()
    }

    /// # Panics
    ///
    /// Panics if this is not the only reference to the underlying files.
    ///
    pub fn into_contents(self) -> HashMap<PathBuf, Content> {
        Rc::try_unwrap(self.files)
            .expect("InMemoryFileSystem::into_files called on a clone")
            .into_inner()
            .into_iter()
            .map(|(path, file)| (path, file.into_content()))
            .collect()
    }

    pub fn paths(&self) -> Vec<PathBuf> {
        self.files.borrow().keys().cloned().collect()
    }

    #[cfg(test)]
    /// Set the modification time of a file.
    ///
    /// Panics if the file does not exist.
    ///
    pub fn set_modification_time(&self, path: &Path, time: SystemTime) {
        self.files
            .deref()
            .borrow_mut()
            .get_mut(path)
            .unwrap()
            .modification_time = time;
    }
}

impl FileSystemIO for InMemoryFileSystem {}

impl FileSystemWriter for InMemoryFileSystem {
    fn delete(&self, path: &Path) -> Result<(), Error> {
        let mut files = self.files.deref().borrow_mut();
        let _ = files.remove(path);
        Ok(())
    }

    fn copy(&self, from: &Path, to: &Path) -> Result<(), Error> {
        self.write_bytes(to, &self.read_bytes(from)?)
    }

    fn copy_dir(&self, _: &Path, _: &Path) -> Result<(), Error> {
        panic!("unimplemented") // TODO
    }

    fn mkdir(&self, _: &Path) -> Result<(), Error> {
        Ok(())
    }

    fn hardlink(&self, _: &Path, _: &Path) -> Result<(), Error> {
        panic!("unimplemented") // TODO
    }

    fn symlink_dir(&self, _: &Path, _: &Path) -> Result<(), Error> {
        panic!("unimplemented") // TODO
    }

    fn delete_file(&self, path: &Path) -> Result<(), Error> {
        let _ = self.files.deref().borrow_mut().remove(path);
        Ok(())
    }

    fn write(&self, path: &Path, content: &str) -> Result<(), Error> {
        self.write_bytes(path, content.as_bytes())
    }

    fn write_bytes(&self, path: &Path, content: &[u8]) -> Result<(), Error> {
        let mut file = InMemoryFile::default();
        _ = io::Write::write(&mut file, content).expect("channel buffer write");
        _ = self
            .files
            .deref()
            .borrow_mut()
            .insert(path.to_path_buf(), file);
        Ok(())
    }
}

impl FileSystemReader for InMemoryFileSystem {
    fn gleam_source_files(&self, dir: &Path) -> Vec<PathBuf> {
        self.files
            .deref()
            .borrow()
            .iter()
            .map(|(file_path, _)| file_path.to_path_buf())
            .filter(|file_path| file_path.starts_with(dir))
            .filter(|file_path| file_path.extension() == Some(OsStr::new("gleam")))
            .collect()
    }

    fn gleam_metadata_files(&self, dir: &Path) -> Vec<PathBuf> {
        self.files
            .deref()
            .borrow()
            .iter()
            .map(|(file_path, _)| file_path.to_path_buf())
            .filter(|file_path| file_path.starts_with(dir))
            .filter(|file_path| file_path.extension() == Some(OsStr::new("gleam_module")))
            .collect()
    }

    fn read(&self, path: &Path) -> Result<String, Error> {
        let path = path.to_path_buf();
        let files = self.files.deref().borrow();
        let file = files.get(&path).ok_or_else(|| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Open,
            path: path.clone(),
            err: None,
        })?;
        let bytes = file.buffer.borrow();
        let unicode = String::from_utf8(bytes.clone()).map_err(|err| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Read,
            path: path.clone(),
            err: Some(err.to_string()),
        })?;
        Ok(unicode)
    }

    fn read_bytes(&self, path: &Path) -> Result<Vec<u8>, Error> {
        let path = path.to_path_buf();
        let files = self.files.deref().borrow();
        let file = files.get(&path).ok_or_else(|| Error::FileIo {
            kind: FileKind::File,
            action: FileIoAction::Open,
            path: path.clone(),
            err: None,
        })?;
        let bytes = file.buffer.borrow().clone();
        Ok(bytes)
    }

    fn is_file(&self, path: &Path) -> bool {
        self.files.deref().borrow().contains_key(path)
    }

    fn is_directory(&self, path: &Path) -> bool {
        self.files
            .deref()
            .borrow()
            .keys()
            .any(|file_path| file_path.starts_with(path))
    }

    fn reader(&self, _path: &Path) -> Result<WrappedReader, Error> {
        // TODO
        unreachable!("Memory reader unimplemented")
    }

    fn read_dir(&self, path: &Path) -> Result<ReadDir> {
        let read_dir = ReadDir::from_iter(
            self.files
                .deref()
                .borrow()
                .iter()
                .map(|(file_path, _)| file_path.to_path_buf())
                .filter(|file_path| file_path.starts_with(path))
                .map(DirEntry::from_pathbuf)
                .map(Ok),
        );

        Ok(read_dir)
    }

    fn current_dir(&self) -> Result<PathBuf, Error> {
        Ok(PathBuf::from("/"))
    }

    fn modification_time(&self, path: &Path) -> Result<SystemTime, Error> {
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

// An in memory sharable that can be used in place of a real file. It is a
// shared reference to a buffer than can be cheaply cloned, all resulting copies
// pointing to the same internal buffer.
//
// Useful in tests and in environments like the browser where there is no file
// system.
//
// Not thread safe. The compiler is single threaded, so that's OK.
//
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InMemoryFile {
    buffer: Rc<RefCell<Vec<u8>>>,
    modification_time: SystemTime,
}

impl InMemoryFile {
    /// # Panics
    ///
    /// Panics if this is not the only reference to the underlying files.
    ///
    pub fn into_content(self) -> Content {
        let contents = Rc::try_unwrap(self.buffer)
            .expect("InMemoryFile::into_content called with multiple references")
            .into_inner();
        match String::from_utf8(contents) {
            Ok(s) => Content::Text(s),
            Err(e) => Content::Binary(e.into_bytes()),
        }
    }
}

impl Default for InMemoryFile {
    fn default() -> Self {
        Self {
            buffer: Default::default(),
            // We use a fixed time here so that the tests are deterministic. In
            // future we may want to inject this in some fashion.
            modification_time: SystemTime::UNIX_EPOCH + Duration::from_secs(663112800),
        }
    }
}

impl std::io::Write for InMemoryFile {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut reference = (*self.buffer).borrow_mut();
        reference.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        let mut reference = (*self.buffer).borrow_mut();
        reference.flush()
    }
}

impl CommandExecutor for InMemoryFileSystem {
    fn exec(
        &self,
        _program: &str,
        _args: &[String],
        _env: &[(&str, String)],
        _cwd: Option<&Path>,
        _stdio: Stdio,
    ) -> Result<i32, Error> {
        Ok(0) // Always succeed.
    }
}
