use super::*;
use std::{cell::RefCell, collections::HashMap, ffi::OsStr, rc::Rc};

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
}

impl FileSystemIO for InMemoryFileSystem {}

impl FileSystemWriter for InMemoryFileSystem {
    fn delete(&self, path: &Path) -> Result<(), Error> {
        let mut files = (*self.files).borrow_mut();
        let _ = files.remove(path);
        Ok(())
    }

    fn copy(&self, _from: &Path, _to: &Path) -> Result<(), Error> {
        panic!("unimplemented") // TODO
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

    fn delete_file(&self, _: &Path) -> Result<(), Error> {
        panic!("unimplemented") // TODO
    }

    fn write(&self, path: &Path, content: &str) -> Result<(), Error> {
        self.write_bytes(path, content.as_bytes())
    }

    fn write_bytes(&self, path: &Path, content: &[u8]) -> Result<(), Error> {
        let mut file = InMemoryFile::default();
        _ = io::Write::write(&mut file, content).expect("channel buffer write");
        _ = (*self.files).borrow_mut().insert(path.to_path_buf(), file);
        Ok(())
    }
}

impl FileSystemReader for InMemoryFileSystem {
    fn gleam_source_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        #[allow(clippy::needless_collect)] // to make borrow work. FIXME.
        let files: Vec<PathBuf> = (*self.files)
            .borrow()
            .iter()
            .map(|(file_path, _)| file_path.to_path_buf())
            .filter(|file_path| file_path.starts_with(dir))
            .filter(|file_path| file_path.extension() == Some(OsStr::new("gleam")))
            .collect();
        Box::new(files.into_iter())
    }

    fn gleam_metadata_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        #[allow(clippy::needless_collect)] // to make borrow work. FIXME.
        let files: Vec<PathBuf> = (*self.files)
            .borrow()
            .iter()
            .map(|(file_path, _)| file_path.to_path_buf())
            .filter(|file_path| file_path.starts_with(dir))
            .filter(|file_path| file_path.extension() == Some(OsStr::new("gleam_module")))
            .collect();
        Box::new(files.into_iter())
    }

    fn read(&self, path: &Path) -> Result<String, Error> {
        let path = path.to_path_buf();
        let files = (*self.files).borrow();
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

    fn is_file(&self, path: &Path) -> bool {
        (*self.files).borrow().contains_key(path)
    }

    fn is_directory(&self, _path: &Path) -> bool {
        unreachable!() // TODO
    }

    fn reader(&self, _path: &Path) -> Result<WrappedReader, Error> {
        unreachable!() // TODO
    }

    fn read_dir(&self, path: &Path) -> Result<ReadDir> {
        let read_dir = ReadDir::from_iter(
            (*self.files)
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
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct InMemoryFile {
    buffer: Rc<RefCell<Vec<u8>>>,
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

// In tests the in memory file system can also be used as a command executor.
#[cfg(test)]
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
