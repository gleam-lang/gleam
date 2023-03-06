use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    time::SystemTime,
};

use gleam_core::{
    io::{
        memory::InMemoryFileSystem, CommandExecutor, FileSystemIO, FileSystemReader,
        FileSystemWriter, ReadDir, Stdio, WrappedReader,
    },
    Error, Result,
};
use im::HashSet;

use crate::fs::ProjectIO;

#[derive(Debug, Clone)]
pub struct LspFsProxy {
    project_io: ProjectIO,
    cache: InMemoryFileSystem,
}

impl LspFsProxy {
    pub fn new() -> Self {
        Self {
            project_io: ProjectIO::new(),
            cache: InMemoryFileSystem::new(),
        }
    }

    pub fn write_mem_cache(&mut self, path: &Path, content: &str) -> Result<(), Error> {
        // println!("Writing cache for {}", path.to_string_lossy());
        tracing::info!("Writing file to cache: {}", path.to_string_lossy());
        self.cache.write(path, content)
    }

    pub fn delete_mem_cache(&self, path: &Path) -> Result<(), Error> {
        tracing::info!("Delete file from cache: {}", path.to_string_lossy());
        self.cache.delete(path)
    }

    pub fn clear_mem_cache(&mut self) {
        self.cache = InMemoryFileSystem::new();
    }
}

impl FileSystemIO for LspFsProxy {}

impl FileSystemWriter for LspFsProxy {
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

impl FileSystemReader for LspFsProxy {
    fn gleam_source_files(&self, dir: &Path) -> Vec<PathBuf> {
        self.project_io.gleam_source_files(dir)
    }

    fn gleam_cache_files(&self, dir: &Path) -> Vec<PathBuf> {
        self.project_io.gleam_cache_files(dir)
    }

    fn read_dir(&self, path: &Path) -> Result<ReadDir> {
        self.project_io.read_dir(path)
    }

    fn read(&self, path: &Path) -> Result<String, Error> {
        let in_mem_result = self.cache.read(path.canonicalize().unwrap().as_path());
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
        let in_mem_result = self
            .cache
            .read_bytes(path.canonicalize().unwrap().as_path());
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

    fn reader(&self, path: &Path) -> Result<WrappedReader, Error> {
        self.project_io.reader(path)
    }

    fn is_file(&self, path: &Path) -> bool {
        self.project_io.is_file(path)
    }

    fn is_directory(&self, path: &Path) -> bool {
        self.project_io.is_directory(path)
    }

    fn current_dir(&self) -> Result<PathBuf, Error> {
        self.project_io.current_dir()
    }

    fn modification_time(&self, path: &Path) -> Result<SystemTime, Error> {
        self.project_io.modification_time(path)
    }
}

impl CommandExecutor for LspFsProxy {
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
