use std::{
    path::{Path, PathBuf},
    time::SystemTime,
};

use gleam_core::{
    error::FileKind,
    io::{
        memory::InMemoryFileSystem, CommandExecutor, FileSystemIO, FileSystemReader,
        FileSystemWriter, ReadDir, Stdio, WrappedReader,
    },
    Error, Result,
};

use crate::fs::ProjectIO;

// A proxy intended for `LanguageServer` to use when files are modified without
// saving.
//
// Uses `ProjectIO` for writing directly to disk, or `InMemoryFileSystem` to
// cache files that were not yet saved. Reading files will always first try the
// `InMemoryFileSystem` first and fallback to use the `ProjectIO` if the file
// was not found in the cache.
//
#[derive(Debug, Clone)]
pub struct FileSystemProxy {
    project_io: ProjectIO,
    cache: InMemoryFileSystem,
}

impl FileSystemProxy {
    pub fn new() -> Self {
        Self {
            project_io: ProjectIO::new(),
            cache: InMemoryFileSystem::new(),
        }
    }

    pub fn write_mem_cache(&mut self, path: &Path, content: &str) -> Result<(), Error> {
        tracing::info!("Writing file to cache: {}", path.to_string_lossy());
        let write_result = self.cache.write(path, content);
        self.cache
            .try_set_modification_time(path, SystemTime::now())?;
        write_result
    }

    pub fn delete_mem_cache(&self, path: &Path) -> Result<(), Error> {
        tracing::info!("Delete file from cache: {}", path.to_string_lossy());
        self.cache.delete(path)
    }
}

impl FileSystemIO for FileSystemProxy {}

// All write operations goes to disk (for mem-cache use the dedicated `_mem_cache` methods)
impl FileSystemWriter for FileSystemProxy {
    fn mkdir(&self, path: &Path) -> Result<(), Error> {
        self.project_io.mkdir(path)
    }

    fn write(&self, path: &Path, content: &str) -> Result<(), Error> {
        self.project_io.write(path, content)
    }

    fn write_bytes(&self, path: &Path, content: &[u8]) -> Result<(), Error> {
        self.project_io.write_bytes(path, content)
    }

    fn delete(&self, path: &Path) -> Result<(), Error> {
        self.project_io.delete(path)
    }

    fn copy(&self, from: &Path, to: &Path) -> Result<(), Error> {
        self.project_io.copy(from, to)
    }

    fn copy_dir(&self, from: &Path, to: &Path) -> Result<(), Error> {
        self.project_io.copy_dir(from, to)
    }

    fn hardlink(&self, from: &Path, to: &Path) -> Result<(), Error> {
        self.project_io.hardlink(from, to)
    }

    fn symlink_dir(&self, from: &Path, to: &Path) -> Result<(), Error> {
        self.project_io.symlink_dir(from, to)
    }

    fn delete_file(&self, path: &Path) -> Result<(), Error> {
        self.project_io.delete_file(path)
    }
}

impl FileSystemReader for FileSystemProxy {
    // Read from disk since `LspFsProxy` should never produce any new files
    fn gleam_source_files(&self, dir: &Path) -> Vec<PathBuf> {
        self.project_io.gleam_source_files(dir)
    }

    // Read from disk since `LspFsProxy` should never produce any new files
    fn gleam_cache_files(&self, dir: &Path) -> Vec<PathBuf> {
        self.project_io.gleam_cache_files(dir)
    }

    // Read from disk since `LspFsProxy` never creates any dirs
    fn read_dir(&self, path: &Path) -> Result<ReadDir> {
        self.project_io.read_dir(path)
    }

    fn read(&self, path: &Path) -> Result<String, Error> {
        // Note that files are assumed to be stored under abs-path keys
        let in_mem_result = self.cache.read(abs_path(path)?.as_path());
        match in_mem_result {
            Ok(_) => {
                tracing::info!("Reading file from cache: {}", path.to_string_lossy());
                in_mem_result
            }
            Err(e) => {
                tracing::info!(
                    "Got {} => Reading file from disk: {}",
                    e,
                    path.to_string_lossy()
                );
                self.project_io.read(path)
            }
        }
    }

    fn read_bytes(&self, path: &Path) -> Result<Vec<u8>, Error> {
        // Note that files are assumed to be stored under abs-path keys
        let in_mem_result = self.cache.read_bytes(abs_path(path)?.as_path());
        match in_mem_result {
            Ok(_) => {
                tracing::info!(
                    "Reading (bytes) file from cache: {}",
                    path.to_string_lossy()
                );
                in_mem_result
            }
            Err(e) => {
                tracing::info!(
                    "Got {} => Reading file from disk: {}",
                    e,
                    path.to_string_lossy()
                );
                self.project_io.read_bytes(path)
            }
        }
    }

    /// # Panics
    ///
    /// Panics if this is not the only reference to the underlying files.
    ///
    fn reader(&self, path: &Path) -> Result<WrappedReader, Error> {
        let in_mem_result = self.cache.reader(abs_path(path)?.as_path());
        match in_mem_result {
            Ok(_) => {
                tracing::info!("Creating reader from cache: {}", path.to_string_lossy());
                in_mem_result
            }
            Err(e) => {
                tracing::info!(
                    "Got {} => Creating reader from disk: {}",
                    e,
                    path.to_string_lossy()
                );
                self.project_io.reader(path)
            }
        }
    }

    // Cache overides existence of file
    fn is_file(&self, path: &Path) -> bool {
        match abs_path(path) {
            Ok(path_buf) => match self.cache.is_file(path_buf.as_path()) {
                true => true,
                false => self.project_io.is_file(path),
            },
            _ => self.project_io.is_file(path),
        }
    }

    // Cache overides existence of directory
    fn is_directory(&self, path: &Path) -> bool {
        match abs_path(path) {
            Ok(path_buf) => match self.cache.is_directory(path_buf.as_path()) {
                true => true,
                false => self.project_io.is_directory(path),
            },
            _ => self.project_io.is_directory(path),
        }
    }

    // Not applicable for mem-cache
    fn current_dir(&self) -> Result<PathBuf, Error> {
        self.project_io.current_dir()
    }

    fn modification_time(&self, path: &Path) -> Result<SystemTime, Error> {
        let in_mem_result = self.cache.modification_time(abs_path(path)?.as_path());
        match in_mem_result {
            Ok(time) => {
                tracing::info!(
                    "Reading modification time {:?} from cache: {}",
                    time,
                    path.to_string_lossy()
                );
                in_mem_result
            }
            Err(e) => {
                let time = self.project_io.modification_time(path);
                tracing::info!(
                    "Got {} => Reading modification time {:?} from disk: {}",
                    e,
                    time,
                    path.to_string_lossy()
                );
                time
            }
        }
    }
}

// Proxy to `ProjectIO` - not applicable for mem-cache
impl CommandExecutor for FileSystemProxy {
    fn exec(
        &self,
        program: &str,
        args: &[String],
        env: &[(&str, String)],
        cwd: Option<&Path>,
        stdio: Stdio,
    ) -> Result<i32, Error> {
        self.project_io.exec(program, args, env, cwd, stdio)
    }
}

// Helper method for getting abs-path
fn abs_path(path: &Path) -> Result<PathBuf, Error> {
    let abs_path = path.canonicalize().or(Err(Error::FileIo {
        kind: FileKind::File,
        action: gleam_core::error::FileIoAction::Canonicalise,
        path: path.to_path_buf(),
        err: None,
    }))?;
    Ok(abs_path)
}
