use crate::{
    config::PackageConfig,
    error::{FileIoAction, FileKind},
    io::{CommandExecutor, FileSystemReader, FileSystemWriter},
    language_server::{
        engine::LanguageServerEngine, files::FileSystemProxy, progress::ProgressReporter,
        DownloadDependencies, MakeLocker,
    },
    paths::ProjectPaths,
    Error, Result,
};
use std::{
    collections::{hash_map::Entry, HashMap},
    path::{Path, PathBuf},
};

pub struct Router<'a, IO> {
    io: FileSystemProxy<IO>,
    engines: HashMap<PathBuf, LanguageServerEngine<'a, IO>>,
    progress_reporter: ProgressReporter<'a>,
}

impl<'a, IO> Router<'a, IO>
where
    IO: FileSystemReader
        + FileSystemWriter
        + CommandExecutor
        + DownloadDependencies
        + MakeLocker
        + Clone,
{
    pub fn new(progress_reporter: ProgressReporter<'a>, io: FileSystemProxy<IO>) -> Self {
        Self {
            io,
            engines: HashMap::new(),
            progress_reporter,
        }
    }

    pub fn engine_for_path(
        &mut self,
        path: &Path,
    ) -> Result<Option<&mut LanguageServerEngine<'a, IO>>> {
        let path = match find_gleam_project_parent(&self.io, path) {
            Some(x) => x,
            None => return Ok(None),
        };

        let entry = match self.engines.entry(path.to_path_buf()) {
            Entry::Occupied(entry) => return Ok(Some(entry.into_mut())),
            Entry::Vacant(entry) => entry,
        };

        let paths = ProjectPaths::new(path.to_path_buf());
        let toml = self.io.read(&path)?;
        let config = toml::from_str(&toml).map_err(|e| Error::FileIo {
            action: FileIoAction::Parse,
            kind: FileKind::File,
            path: path.to_path_buf(),
            err: Some(e.to_string()),
        })?;
        let engine = LanguageServerEngine::new(
            Some(config),
            self.progress_reporter.clone(),
            self.io.clone(),
            paths,
        )?;
        Ok(Some(entry.insert(engine)))
    }
}

/// Given a path, find the nearest parent directory containing a `gleam.toml`
/// file.
fn find_gleam_project_parent<IO>(io: &IO, path: &Path) -> Option<PathBuf>
where
    IO: FileSystemReader,
{
    let mut current = path.to_path_buf();
    while current.pop() {
        if io.is_file(&current.join("gleam.toml")) {
            return Some(current);
        }
    }
    None
}

#[cfg(test)]
mod find_gleam_project_parent_tests {
    use super::*;
    use crate::io::{memory::InMemoryFileSystem, FileSystemWriter};

    #[test]
    fn root() {
        let io = InMemoryFileSystem::new();
        assert_eq!(find_gleam_project_parent(&io, Path::new("/")), None);
    }

    #[test]
    fn outside_a_project() {
        let io = InMemoryFileSystem::new();
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/src/one.gleam")),
            None
        );
    }

    #[test]
    fn gleam_toml_itself() {
        let io = InMemoryFileSystem::new();
        io.write(Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/gleam.toml")),
            Some(PathBuf::from("/app"))
        );
    }

    #[test]
    fn test_module() {
        let io = InMemoryFileSystem::new();
        io.write(Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/test/one/two/three.gleam")),
            Some(PathBuf::from("/app"))
        );
    }

    #[test]
    fn src_module() {
        let io = InMemoryFileSystem::new();
        io.write(Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/src/one/two/three.gleam")),
            Some(PathBuf::from("/app"))
        );
    }

    #[test]
    fn nested_projects() {
        let io = InMemoryFileSystem::new();
        io.write(Path::new("/app/gleam.toml"), "").unwrap();
        io.write(Path::new("/app/examples/wibble/gleam.toml"), "")
            .unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/src/one.gleam")),
            Some(PathBuf::from("/app"))
        );
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/examples/wibble/src/one.gleam")),
            Some(PathBuf::from("/app/examples/wibble"))
        );
    }
}
