use gleam_core::{
    io::{
        memory::InMemoryFileSystem, CommandExecutor, FileSystemIO, FileSystemReader,
        FileSystemWriter, ReadDir, Stdio, WrappedReader,
    },
    Error, Result,
};
use std::path::{Path, PathBuf};

#[derive(Clone, Debug)]
pub struct WasmFileSystem {
    imfs: InMemoryFileSystem,
}

impl WasmFileSystem {
    pub fn new() -> WasmFileSystem {
        WasmFileSystem {
            imfs: InMemoryFileSystem::new(),
        }
    }
}

impl CommandExecutor for WasmFileSystem {
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

impl FileSystemIO for WasmFileSystem {}

impl FileSystemWriter for WasmFileSystem {
    fn delete(&self, path: &Path) -> Result<(), Error> {
        tracing::trace!("delete {:?}", path);
        self.imfs.delete(path)
    }

    fn copy(&self, _from: &Path, _to: &Path) -> Result<(), Error> {
        Ok(())
    }
    fn copy_dir(&self, _: &Path, _: &Path) -> Result<(), Error> {
        Ok(())
    }

    fn mkdir(&self, _: &Path) -> Result<(), Error> {
        Ok(())
    }

    fn hardlink(&self, _: &Path, _: &Path) -> Result<(), Error> {
        Ok(())
    }

    fn symlink_dir(&self, _: &Path, _: &Path) -> Result<(), Error> {
        Ok(())
    }

    fn delete_file(&self, path: &Path) -> Result<(), Error> {
        tracing::trace!("delete file {:?}", path);
        self.imfs.delete_file(path)
    }

    fn write(&self, path: &Path, content: &str) -> Result<(), Error> {
        tracing::trace!("write {:?}", path);
        self.imfs.write(path, content)
    }

    fn write_bytes(&self, path: &Path, content: &[u8]) -> Result<(), Error> {
        tracing::trace!("write_bytes {:?}", path);
        self.imfs.write_bytes(path, content)
    }
}

impl FileSystemReader for WasmFileSystem {
    fn gleam_source_files(&self, dir: &Path) -> Vec<PathBuf> {
        tracing::trace!("gleam_source_files   {:?}", dir);
        self.imfs.gleam_source_files(dir)
    }

    fn gleam_metadata_files(&self, dir: &Path) -> Vec<PathBuf> {
        tracing::trace!("gleam_metadata_files {:?}", dir);
        self.imfs.gleam_metadata_files(dir)
    }

    fn read(&self, path: &Path) -> Result<String, Error> {
        tracing::trace!("read {:?}", path);
        self.imfs.read(path)
    }

    fn is_file(&self, path: &Path) -> bool {
        tracing::info!("is_file {:?}", path);
        self.imfs.is_file(path)
    }

    fn is_directory(&self, path: &Path) -> bool {
        tracing::trace!("is_directory {:?}", path);
        false
    }

    fn reader(&self, path: &Path) -> Result<WrappedReader, Error> {
        tracing::trace!("reader {:?}", path);
        self.imfs.reader(path)
    }

    fn read_dir(&self, path: &Path) -> Result<ReadDir> {
        tracing::trace!("read_dir {:?}", path);
        self.imfs.read_dir(path)
    }

    fn current_dir(&self) -> Result<PathBuf, Error> {
        self.imfs.current_dir()
    }

    fn modification_time(&self, path: &Path) -> Result<std::time::SystemTime, Error> {
        self.imfs.modification_time(path)
    }

    fn read_bytes(&self, path: &Path) -> Result<Vec<u8>, Error> {
        self.imfs.read_bytes(path)
    }
}
