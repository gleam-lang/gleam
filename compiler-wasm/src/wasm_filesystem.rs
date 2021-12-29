use gleam_core::{
    io::{
        memory::InMemoryFileSystem, CommandExecutor, FileSystemIO, FileSystemReader,
        FileSystemWriter, ReadDir, WrappedReader, WrappedWriter,
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
    ) -> Result<i32, Error> {
        Ok(0) // Always succeed.
    }
}

impl FileSystemIO for WasmFileSystem {}

impl FileSystemWriter for WasmFileSystem {
    fn writer(&self, path: &Path) -> Result<WrappedWriter, Error> {
        log::info!("\x1b[93mwrite\x1b[0m                {:?}", path);
        self.imfs.writer(path)
    }

    fn delete(&self, path: &Path) -> Result<(), Error> {
        log::info!("\x1b[31mdelete\x1b[0m               {:?}", path);
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
}

impl FileSystemReader for WasmFileSystem {
    fn gleam_source_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        log::info!("\x1b[32mgleam_source_files\x1b[0m   {:?}", dir);
        self.imfs.gleam_source_files(dir)
    }

    fn gleam_metadata_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        log::info!("\x1b[32mgleam_metadata_files\x1b[0m {:?}", dir);
        self.imfs.gleam_metadata_files(dir)
    }

    fn read(&self, path: &Path) -> Result<String, Error> {
        log::info!("\x1b[32mread\x1b[0m                 {:?}", path);
        self.imfs.read(path)
    }

    fn is_file(&self, path: &Path) -> bool {
        log::info!("\x1b[32mis_file\x1b[0m              {:?}", path);
        self.imfs.is_file(path)
    }

    fn is_directory(&self, path: &Path) -> bool {
        log::info!("\x1b[32mis_directory\x1b[0m         {:?}", path);
        false
    }

    fn reader(&self, path: &Path) -> Result<WrappedReader, Error> {
        log::info!("\x1b[93mreader\x1b[0m               {:?}", path);
        self.imfs.reader(path)
    }

    fn read_dir(&self, path: &Path) -> Result<ReadDir> {
        log::info!("\x1b[93mread_dir\x1b[0m             {:?}", path);
        self.imfs.read_dir(path)
    }
}
