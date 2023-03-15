use std::{
    path::{Path, PathBuf},
    time::SystemTime,
};

use debug_ignore::DebugIgnore;

use crate::{
    error::{FileIoAction, FileKind},
    io::{
        memory::InMemoryFileSystem, CommandExecutor, FileSystemReader, FileSystemWriter, ReadDir,
        Stdio, WrappedReader,
    },
    Error, Result,
};

// A proxy intended for `LanguageServer` to use when files are modified in
// memory but not yet saved to disc by the client.
//
// Uses the `IO` for writing directly to disk, or `InMemoryFileSystem` to
// cache files that were not yet saved. Reading files will always first try the
// `InMemoryFileSystem` first and fallback to use the `ProjectIO` if the file
// was not found in the cache.
//
#[derive(Debug, Clone)]
pub struct FileSystemProxy<IO> {
    io: DebugIgnore<IO>,
    edit_cache: InMemoryFileSystem,
}

impl<IO> FileSystemProxy<IO>
where
    IO: FileSystemWriter + FileSystemReader + CommandExecutor,
{
    pub fn new(io: IO) -> Self {
        Self {
            io: io.into(),
            edit_cache: InMemoryFileSystem::new(),
        }
    }

    pub fn write_mem_cache(&mut self, path: &Path, content: &str) -> Result<()> {
        let write_result = self.edit_cache.write(path, content);
        self.edit_cache
            .try_set_modification_time(path, SystemTime::now())?;
        write_result
    }

    pub fn delete_mem_cache(&self, path: &Path) -> Result<()> {
        self.edit_cache.delete(path)
    }
}

// All write operations goes to disk (for mem-cache use the dedicated `_mem_cache` methods)
impl<IO> FileSystemWriter for FileSystemProxy<IO>
where
    IO: FileSystemWriter,
{
    fn mkdir(&self, path: &Path) -> Result<()> {
        self.io.mkdir(path)
    }

    fn write(&self, path: &Path, content: &str) -> Result<()> {
        self.io.write(path, content)
    }

    fn write_bytes(&self, path: &Path, content: &[u8]) -> Result<()> {
        self.io.write_bytes(path, content)
    }

    fn delete(&self, path: &Path) -> Result<()> {
        self.io.delete(path)
    }

    fn copy(&self, from: &Path, to: &Path) -> Result<()> {
        self.io.copy(from, to)
    }

    fn copy_dir(&self, from: &Path, to: &Path) -> Result<()> {
        self.io.copy_dir(from, to)
    }

    fn hardlink(&self, from: &Path, to: &Path) -> Result<()> {
        self.io.hardlink(from, to)
    }

    fn symlink_dir(&self, from: &Path, to: &Path) -> Result<()> {
        self.io.symlink_dir(from, to)
    }

    fn delete_file(&self, path: &Path) -> Result<()> {
        self.io.delete_file(path)
    }
}

impl<IO> FileSystemReader for FileSystemProxy<IO>
where
    IO: FileSystemReader,
{
    fn gleam_source_files(&self, dir: &Path) -> Vec<PathBuf> {
        self.io.gleam_source_files(dir)
    }

    fn gleam_cache_files(&self, dir: &Path) -> Vec<PathBuf> {
        self.io.gleam_cache_files(dir)
    }

    fn read_dir(&self, path: &Path) -> Result<ReadDir> {
        self.io.read_dir(path)
    }

    fn read(&self, path: &Path) -> Result<String> {
        // Note that files are assumed to be stored under abs-path keys
        let in_mem_result = self.edit_cache.read(abs_path(path)?.as_path());
        match in_mem_result {
            Ok(_) => in_mem_result,
            Err(_) => self.io.read(path),
        }
    }

    fn read_bytes(&self, path: &Path) -> Result<Vec<u8>> {
        // Note that files are assumed to be stored under abs-path keys
        let in_mem_result = self.edit_cache.read_bytes(abs_path(path)?.as_path());
        match in_mem_result {
            Ok(_) => in_mem_result,
            Err(_) => self.io.read_bytes(path),
        }
    }

    fn reader(&self, path: &Path) -> Result<WrappedReader> {
        self.io.reader(path)
    }

    // Cache overides existence of file
    fn is_file(&self, path: &Path) -> bool {
        match abs_path(path) {
            Ok(path_buf) => match self.edit_cache.is_file(path_buf.as_path()) {
                true => true,
                false => self.io.is_file(path),
            },
            _ => self.io.is_file(path),
        }
    }

    // Cache overides existence of directory
    fn is_directory(&self, path: &Path) -> bool {
        match abs_path(path) {
            Ok(path_buf) => match self.edit_cache.is_directory(path_buf.as_path()) {
                true => true,
                false => self.io.is_directory(path),
            },
            _ => self.io.is_directory(path),
        }
    }

    fn modification_time(&self, path: &Path) -> Result<SystemTime> {
        let in_mem_result = self.edit_cache.modification_time(abs_path(path)?.as_path());
        match in_mem_result {
            Ok(_) => in_mem_result,
            Err(_) => self.io.modification_time(path),
        }
    }
}

// Proxy to `ProjectIO` - not applicable for mem-cache
impl<IO> CommandExecutor for FileSystemProxy<IO>
where
    IO: CommandExecutor,
{
    fn exec(
        &self,
        program: &str,
        args: &[String],
        env: &[(&str, String)],
        cwd: Option<&Path>,
        stdio: Stdio,
    ) -> Result<i32> {
        self.io.exec(program, args, env, cwd, stdio)
    }
}

// Helper method for getting abs-path
fn abs_path(path: &Path) -> Result<PathBuf> {
    let abs_path = path.canonicalize().or(Err(Error::FileIo {
        kind: FileKind::File,
        action: FileIoAction::Canonicalise,
        path: path.to_path_buf(),
        err: None,
    }))?;
    Ok(abs_path)
}
