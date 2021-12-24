use std::{
    ffi::OsStr,
    io,
    path::{Path, PathBuf},
};

use gleam_core::{
    io::{
        memory::InMemoryFileSystem, CommandExecutor, DirEntry, FileSystemIO, FileSystemReader,
        FileSystemWriter, ReadDir, WrappedReader, WrappedWriter,
    },
    Error, Result,
};
use rust_embed::RustEmbed;

use crate::static_files::StaticFiles;

#[derive(RustEmbed)]
#[folder = "static/"]
struct Packages;

#[derive(Clone, Debug)]
pub struct WasmFileSystem {
    imfs: InMemoryFileSystem,
    static_files: StaticFiles,
}

impl WasmFileSystem {
    pub fn new() -> WasmFileSystem {
        WasmFileSystem {
            imfs: InMemoryFileSystem::new(),
            static_files: StaticFiles
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
        println!("wrote to {:?}", path);
        self.imfs.writer(path)
    }

    fn delete(&self, path: &Path) -> Result<(), Error> {
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
        println!("gleam_source_files {:?}", dir);
        let mut files1: Vec<PathBuf> = self.imfs.gleam_source_files(dir).collect();
        let mut files2: Vec<PathBuf> = self.static_files.gleam_source_files(dir).collect();

        files1.append(&mut files2);

        Box::new(files1.into_iter())
    }

    fn gleam_metadata_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        println!("gleam_metadata_files {:?}", dir);
        self.static_files.gleam_metadata_files(dir)
    }

    fn read(&self, path: &Path) -> Result<String, Error> {
        println!("read {:?}", path);
        self.imfs.read(path).or_else(|_error| self.static_files.read(path) )
    }

    fn is_file(&self, path: &Path) -> bool {
        self.imfs.is_file(path) || self.static_files.is_file(path)
    }

    fn is_directory(&self, _path: &Path) -> bool {
        false
    }

    fn reader(&self, path: &Path) -> Result<WrappedReader, Error> {
        self.static_files.reader(path)
    }

    fn read_dir(&self, path: &Path) -> Result<ReadDir> {
        println!("read_dir {:?}", path);

        let read_dir_imfs = self.imfs.read_dir(path)?;
        let read_dir_sf = self.static_files.read_dir(path)?;

        Ok(read_dir_imfs.extend(read_dir_sf))
    }
}
