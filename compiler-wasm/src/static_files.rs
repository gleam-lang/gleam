use std::{
    ffi::OsStr,
    io,
    path::{Path, PathBuf},
};

use gleam_core::{
    error::{FileIoAction, FileKind},
    io::{DirEntry, FileSystemReader, ReadDir, WrappedReader},
    Error, Result,
};
use rust_embed::RustEmbed;

#[derive(RustEmbed, Debug, Clone)]
#[folder = "static/"]
pub struct StaticFiles;

impl FileSystemReader for StaticFiles {
    fn gleam_source_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        let files: Vec<PathBuf> = self
            .read_dir(dir)
            .unwrap()
            .filter_map(|x| x.ok())
            .map(|x| x.pathbuf)
            .filter(|file_path| file_path.extension() == Some(OsStr::new("gleam")))
            .collect();

        Box::new(files.into_iter())
    }

    fn gleam_metadata_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        let files: Vec<PathBuf> = self
            .read_dir(dir)
            .unwrap()
            .filter_map(|x| x.ok())
            .map(|x| x.pathbuf)
            .filter(|file_path| file_path.extension() == Some(OsStr::new("gleam_module")))
            .collect();

        Box::new(files.into_iter())
    }

    fn read(&self, path: &Path) -> Result<String, Error> {
        if let Some(package) = StaticFiles::get(path.to_str().unwrap()) {
            let bytes = package.data.to_vec();
            let unicode = String::from_utf8(bytes.clone()).map_err(|err| Error::FileIo {
                kind: FileKind::File,
                action: FileIoAction::Read,
                path: path.to_owned(),
                err: Some(err.to_string()),
            })?;
            Ok(unicode)
        } else {
            Err(Error::FileIo {
                kind: FileKind::File,
                action: FileIoAction::Read,
                path: path.to_owned(),
                err: Some(String::from("File not found")),
            })
        }
    }

    fn is_file(&self, path: &Path) -> bool {
        self.read(path).is_ok()
    }

    fn is_directory(&self, _path: &Path) -> bool {
        false
    }

    fn reader(&self, path: &Path) -> Result<WrappedReader, Error> {
        self.read(path)
            .map(|result| WrappedReader::new(path, Box::new(io::Cursor::new(result))))
    }

    fn read_dir(&self, path: &Path) -> Result<ReadDir> {
        let files: Vec<io::Result<DirEntry>> = StaticFiles::iter()
            .map(|x| PathBuf::from(x.as_ref()))
            .filter(|file_path| file_path.starts_with(path))
            .map(DirEntry::from_pathbuf)
            .map(Ok)
            .collect();

        Ok(ReadDir::from_entries(files))
    }
}
