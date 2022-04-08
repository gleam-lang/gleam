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
#[derive(Clone, Default, Debug, PartialEq)]
pub struct InMemoryFileSystem {
    files: Rc<RefCell<HashMap<PathBuf, InMemoryFile>>>,
}

impl InMemoryFileSystem {
    pub fn new() -> Self {
        Self::default()
    }
}

impl FileSystemIO for InMemoryFileSystem {}

impl FileSystemWriter for InMemoryFileSystem {
    fn writer(&self, path: &Path) -> Result<WrappedWriter, Error> {
        let mut files = (*self.files).borrow_mut();
        let writer = files.entry(path.to_path_buf()).or_default();
        Ok(WrappedWriter {
            path: path.to_path_buf(),
            inner: DebugIgnore(Box::new(writer.clone())),
        })
    }

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
        panic!("unimplemented") // TODO
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
#[derive(Debug, Clone, PartialEq, Default)]
pub struct InMemoryFile {
    buffer: Rc<RefCell<Vec<u8>>>,
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
