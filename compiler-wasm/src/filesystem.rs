use std::{
    ffi::OsStr,
    io,
    path::{Path, PathBuf},
    process::ExitStatus,
};

use gleam_core::{
    error::{FileIoAction, FileKind},
    io::{
        memory::InMemoryFileSystem, CommandExecutor, DirEntry, FileSystemIO, FileSystemReader,
        FileSystemWriter, ReadDir, WrappedReader, WrappedWriter,
    },
    Error, Result,
};
use rust_embed::RustEmbed;

#[derive(RustEmbed)]
#[folder = "static/"]
struct Packages;

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
    ) -> Result<ExitStatus, Error> {
        panic!("Not implemented")
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
        panic!("unimplemented");
    }
    fn copy_dir(&self, _: &Path, _: &Path) -> Result<(), Error> {
        panic!("unimplemented");
    }

    fn mkdir(&self, _: &Path) -> Result<(), Error> {
        panic!("unimplemented");
    }
}

impl FileSystemReader for WasmFileSystem {
    fn gleam_source_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        println!(
            "gleam_source_files {:?}",
            dir
        );
        let mut files1: Vec<PathBuf> = self.imfs.gleam_source_files(dir).collect();

        let mut files2: Vec<PathBuf> = Packages::iter()
            .map(|x| {
                let path_os_str = OsStr::new(x.as_ref());
                Path::new(path_os_str).to_path_buf()
            })
            .filter(|file_path| file_path.starts_with(dir))
            .filter(|file_path| file_path.extension() == Some(OsStr::new("gleam")))
            .collect();

        files1.append(&mut files2);

        Box::new(files1.into_iter())
    }

    fn gleam_metadata_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        println!("gleam_metadata_files {:?}", dir);
        let files: Vec<PathBuf> = Packages::iter()
            .map(|x| {
                let path_os_str = OsStr::new(x.as_ref());
                Path::new(path_os_str).to_path_buf()
            })
            .filter(|file_path| file_path.starts_with(dir))
            .filter(|file_path| file_path.extension() == Some(OsStr::new("gleam_module")))
            .collect();

        Box::new(files.into_iter())
    }

    fn read(&self, path: &Path) -> Result<String, Error> {
        println!("read {:?}", path);
        self.imfs.read(path).or_else(|error| {
            if let Some(package) = Packages::get(path.to_str().unwrap()) {
                let bytes = package.data.to_vec();
                let unicode = String::from_utf8(bytes.clone()).map_err(|err| Error::FileIo {
                    kind: FileKind::File,
                    action: FileIoAction::Read,
                    path: path.to_owned(),
                    err: Some(err.to_string()),
                })?;
                Ok(unicode)
            } else {
                Err(error)
            }
        })
    }

    fn is_file(&self, path: &Path) -> bool {
        self.imfs.is_file(path)
    }

    fn is_directory(&self, _path: &Path) -> bool {
        false // The directory is a lie!
    }

    fn reader(&self, path: &Path) -> Result<WrappedReader, Error> {
        if let Some(file) = Packages::get(path.as_os_str().to_str().unwrap()) {
            let bytes = std::io::Cursor::new(file.data.clone().to_vec());

            return Ok(WrappedReader::new(path, Box::new(bytes)));
        }

        unimplemented!()
    }

    fn read_dir(&self, path: &Path) -> Result<ReadDir> {
        println!("read_dir {:?}", path);
        let mut files: Vec<io::Result<DirEntry>> = Packages::iter()
            .map(|x| {
                let path_os_str = OsStr::new(x.as_ref());
                Path::new(path_os_str).to_path_buf()
            })
            .filter(|file_path| file_path.starts_with(path))
            .map(DirEntry::from_pathbuf)
            .map(Ok)
            .collect();

        let mut files2: Vec<io::Result<DirEntry>> =
            self.imfs.read_dir(path).unwrap().into_iter().collect();

        files.append(&mut files2);

        Ok(ReadDir::from_entries(files))
    }
}
