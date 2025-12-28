use std::collections::HashSet;
use std::time::SystemTime;

use debug_ignore::DebugIgnore;

use gleam_core::{
    Result,
    error::Error,
    io::{
        BeamCompiler, Command, CommandExecutor, FileSystemReader, FileSystemWriter, ReadDir, Stdio,
        WrappedReader, memory::InMemoryFileSystem,
    },
};

use camino::{Utf8Path, Utf8PathBuf};

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

    pub fn inner(&self) -> &IO {
        &self.io
    }

    pub fn write_mem_cache(&mut self, path: &Utf8Path, content: &str) -> Result<()> {
        let write_result = self.edit_cache.write(path, content);
        self.edit_cache
            .try_set_modification_time(path, SystemTime::now())?;
        write_result
    }

    pub fn delete_mem_cache(&self, path: &Utf8Path) -> Result<()> {
        if self.edit_cache.is_directory(path) {
            self.edit_cache.delete_directory(path)
        } else {
            self.edit_cache.delete_file(path)
        }
    }
}

// All write operations goes to disk (for mem-cache use the dedicated `_mem_cache` methods)
impl<IO> FileSystemWriter for FileSystemProxy<IO>
where
    IO: FileSystemWriter,
{
    fn mkdir(&self, path: &Utf8Path) -> Result<()> {
        self.io.mkdir(path)
    }

    fn write(&self, path: &Utf8Path, content: &str) -> Result<()> {
        self.io.write(path, content)
    }

    fn write_bytes(&self, path: &Utf8Path, content: &[u8]) -> Result<()> {
        self.io.write_bytes(path, content)
    }

    fn delete_directory(&self, path: &Utf8Path) -> Result<()> {
        self.io.delete_directory(path)
    }

    fn copy(&self, from: &Utf8Path, to: &Utf8Path) -> Result<()> {
        self.io.copy(from, to)
    }

    fn copy_dir(&self, from: &Utf8Path, to: &Utf8Path) -> Result<()> {
        self.io.copy_dir(from, to)
    }

    fn hardlink(&self, from: &Utf8Path, to: &Utf8Path) -> Result<()> {
        self.io.hardlink(from, to)
    }

    fn symlink_dir(&self, from: &Utf8Path, to: &Utf8Path) -> Result<()> {
        self.io.symlink_dir(from, to)
    }

    fn delete_file(&self, path: &Utf8Path) -> Result<()> {
        self.io.delete_file(path)
    }

    fn exists(&self, path: &Utf8Path) -> bool {
        self.io.exists(path)
    }
}

impl<IO> FileSystemReader for FileSystemProxy<IO>
where
    IO: FileSystemReader,
{
    fn read_dir(&self, path: &Utf8Path) -> Result<ReadDir> {
        self.io.read_dir(path)
    }

    fn read(&self, path: &Utf8Path) -> Result<String> {
        match self.edit_cache.read(path) {
            result @ Ok(_) => result,
            Err(_) => self.io.read(path),
        }
    }

    fn read_bytes(&self, path: &Utf8Path) -> Result<Vec<u8>> {
        match self.edit_cache.read_bytes(path) {
            result @ Ok(_) => result,
            Err(_) => self.io.read_bytes(path),
        }
    }

    fn reader(&self, path: &Utf8Path) -> Result<WrappedReader> {
        self.io.reader(path)
    }

    // Cache overrides existence of file
    fn is_file(&self, path: &Utf8Path) -> bool {
        self.edit_cache.is_file(path) || self.io.is_file(path)
    }

    // Cache overrides existence of directory
    fn is_directory(&self, path: &Utf8Path) -> bool {
        self.edit_cache.is_directory(path) || self.io.is_directory(path)
    }

    fn modification_time(&self, path: &Utf8Path) -> Result<SystemTime> {
        match self.edit_cache.modification_time(path) {
            result @ Ok(_) => result,
            Err(_) => self.io.modification_time(path),
        }
    }

    fn canonicalise(&self, path: &Utf8Path) -> Result<Utf8PathBuf, Error> {
        self.io.canonicalise(path)
    }
}

impl<IO> CommandExecutor for FileSystemProxy<IO>
where
    IO: CommandExecutor,
{
    fn exec(&self, _command: Command) -> Result<i32> {
        panic!("The language server is not permitted to create subprocesses")
    }
}

impl<IO> BeamCompiler for FileSystemProxy<IO>
where
    IO: BeamCompiler,
{
    fn compile_beam(
        &self,
        _out: &Utf8Path,
        _lib: &Utf8Path,
        _modules: &HashSet<Utf8PathBuf>,
        _stdio: Stdio,
    ) -> Result<Vec<String>, Error> {
        panic!("The language server is not permitted to create subprocesses")
    }
}
