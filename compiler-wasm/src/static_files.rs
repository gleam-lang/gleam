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
        Box::new(
            self.read_dir(dir)
                .unwrap()
                .into_iter()
                .filter_map(|x| x.ok())
                .map(|x| x.pathbuf)
                .filter(|file_path| file_path.extension() == Some(OsStr::new("gleam"))),
        )
    }

    fn gleam_metadata_files(&self, dir: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        Box::new(
            self.read_dir(dir)
                .unwrap()
                .into_iter()
                .filter_map(|x| x.ok())
                .map(|x| x.pathbuf)
                .filter(|file_path| file_path.extension() == Some(OsStr::new("gleam_module"))),
        )
    }

    fn read(&self, path: &Path) -> Result<String, Error> {
        if let Some(package) = StaticFiles::get(path.to_str().unwrap()) {
            let bytes = package.data.to_vec();
            let unicode = String::from_utf8(bytes).map_err(|err| Error::FileIo {
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
        let read_dir: ReadDir = StaticFiles::iter()
            .map(|x| PathBuf::from(x.as_ref()))
            .filter(|file_path| file_path.starts_with(path))
            .map(DirEntry::from_pathbuf)
            .map(Ok)
            .collect();

        Ok(read_dir)
    }
}
