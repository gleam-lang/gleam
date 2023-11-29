use camino::{Utf8Path, Utf8PathBuf};
use gleam_core::{
    io::{
        memory::InMemoryFileSystem, CommandExecutor, FileSystemReader, FileSystemWriter, ReadDir,
        Stdio, WrappedReader,
    },
    Error, Result,
};

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

    fn mkdir(&self, _: &Utf8Path) -> Result<(), Error> {
        Ok(())
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
}

impl FileSystemReader for WasmFileSystem {
    fn gleam_source_files(&self, dir: &Utf8Path) -> Vec<Utf8PathBuf> {
        tracing::trace!("gleam_source_files   {:?}", dir);
        self.imfs.gleam_source_files(dir)
    }

    fn gleam_cache_files(&self, dir: &Utf8Path) -> Vec<Utf8PathBuf> {
        tracing::trace!("gleam_metadata_files {:?}", dir);
        self.imfs.gleam_cache_files(dir)
    }

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
        false
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
