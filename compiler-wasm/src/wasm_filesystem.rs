use camino::{Utf8Path, Utf8PathBuf};
use gleam_core::{
    io::{
        memory::InMemoryFileSystem, BeamCompiler, CommandExecutor, FileSystemReader,
        FileSystemWriter, ReadDir, Stdio, WrappedReader,
    },
    Error, Result,
};
use std::collections::HashSet;

#[derive(Clone, Debug, Default)]
pub struct WasmFileSystem {
    imfs: InMemoryFileSystem,
}

impl WasmFileSystem {
    pub fn reset(&self) {
        self.imfs.reset();
    }
}

impl CommandExecutor for WasmFileSystem {
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

impl BeamCompiler for WasmFileSystem {
    fn compile_beam(
        &self,
        _out: &Utf8Path,
        _lib: &Utf8Path,
        _modules: &HashSet<Utf8PathBuf>,
        _stdio: Stdio,
    ) -> Result<Vec<String>, Error> {
        Ok(Vec::new()) // Always succeed.
    }
}

impl FileSystemWriter for WasmFileSystem {
    fn delete_directory(&self, path: &Utf8Path) -> Result<(), Error> {
        tracing::trace!("delete {:?}", path);
        self.imfs.delete_directory(path)
    }

    fn copy(&self, _from: &Utf8Path, _to: &Utf8Path) -> Result<(), Error> {
        Ok(())
    }
    fn copy_dir(&self, _: &Utf8Path, _: &Utf8Path) -> Result<(), Error> {
        Ok(())
    }

    fn mkdir(&self, path: &Utf8Path) -> Result<(), Error> {
        tracing::trace!("mkdir {:?}", path);
        self.imfs.mkdir(path)
    }

    fn hardlink(&self, _: &Utf8Path, _: &Utf8Path) -> Result<(), Error> {
        Ok(())
    }

    fn symlink_dir(&self, _: &Utf8Path, _: &Utf8Path) -> Result<(), Error> {
        Ok(())
    }

    fn delete_file(&self, path: &Utf8Path) -> Result<(), Error> {
        tracing::trace!("delete file {:?}", path);
        self.imfs.delete_file(path)
    }

    fn write(&self, path: &Utf8Path, content: &str) -> Result<(), Error> {
        tracing::trace!("write {:?}", path);
        self.imfs.write(path, content)
    }

    fn write_bytes(&self, path: &Utf8Path, content: &[u8]) -> Result<(), Error> {
        tracing::trace!("write_bytes {:?}", path);
        self.imfs.write_bytes(path, content)
    }

    fn exists(&self, path: &Utf8Path) -> bool {
        self.imfs.exists(path)
    }
}

impl FileSystemReader for WasmFileSystem {
    fn read(&self, path: &Utf8Path) -> Result<String, Error> {
        tracing::trace!("read {:?}", path);
        self.imfs.read(path)
    }

    fn is_file(&self, path: &Utf8Path) -> bool {
        tracing::info!("is_file {:?}", path);
        self.imfs.is_file(path)
    }

    fn is_directory(&self, path: &Utf8Path) -> bool {
        tracing::trace!("is_directory {:?}", path);
        self.imfs.is_directory(path)
    }

    fn reader(&self, path: &Utf8Path) -> Result<WrappedReader, Error> {
        tracing::trace!("reader {:?}", path);
        self.imfs.reader(path)
    }

    fn read_dir(&self, path: &Utf8Path) -> Result<ReadDir> {
        tracing::trace!("read_dir {:?}", path);
        self.imfs.read_dir(path)
    }

    fn modification_time(&self, path: &Utf8Path) -> Result<std::time::SystemTime, Error> {
        self.imfs.modification_time(path)
    }

    fn read_bytes(&self, path: &Utf8Path) -> Result<Vec<u8>, Error> {
        self.imfs.read_bytes(path)
    }

    fn canonicalise(&self, path: &Utf8Path) -> Result<Utf8PathBuf, Error> {
        self.imfs.canonicalise(path)
    }
}
