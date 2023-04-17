use crate::{
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

use super::feedback::FeedbackBookKeeper;

/// The language server instance serves a langauge client, typically a text
/// editor. The editor could have multiple Gleam projects open at once, so run
/// an instance of the language server engine for each project.
///
/// This router is responsible for finding or creating an engine for a given
/// file using the nearest parent `gleam.toml` file.
///
#[derive(Debug)]
pub struct Router<IO, Reporter> {
    io: FileSystemProxy<IO>,
    engines: HashMap<PathBuf, Project<IO, Reporter>>,
    progress_reporter: Reporter,
}

impl<'a, IO, Reporter> Router<IO, Reporter>
where
    // IO to be supplied from outside of gleam-core
    IO: FileSystemReader
        + FileSystemWriter
        + CommandExecutor
        + DownloadDependencies
        + MakeLocker
        + Clone,
    // IO to be supplied from inside of gleam-core
    Reporter: ProgressReporter + Clone + 'a,
{
    pub fn new(progress_reporter: Reporter, io: FileSystemProxy<IO>) -> Self {
        Self {
            io,
            engines: HashMap::new(),
            progress_reporter,
        }
    }

    pub fn project_for_path(&mut self, path: &Path) -> Result<Option<&mut Project<IO, Reporter>>> {
        let path = match find_gleam_project_parent(&self.io, path) {
            Some(x) => x,
            None => return Ok(None),
        };

        let entry = match self.engines.entry(path.clone()) {
            Entry::Occupied(entry) => return Ok(Some(entry.into_mut())),
            Entry::Vacant(entry) => entry,
        };

        tracing::info!(?path, "creating_new_language_server_engine");

        let paths = ProjectPaths::new(path);
        let config_path = paths.root_config();
        let toml = self.io.read(&config_path)?;
        let config = toml::from_str(&toml).map_err(|e| Error::FileIo {
            action: FileIoAction::Parse,
            kind: FileKind::File,
            path: config_path,
            err: Some(e.to_string()),
        })?;
        let engine = LanguageServerEngine::new(
            config,
            self.progress_reporter.clone(),
            self.io.clone(),
            paths,
        )?;
        let project = Project {
            engine,
            feedback: FeedbackBookKeeper::default(),
        };
        Ok(Some(entry.insert(project)))
    }

    pub fn delete_engine_for_path(&mut self, path: &Path) {
        if let Some(path) = find_gleam_project_parent(&self.io, path) {
            _ = self.engines.remove(&path);
        }
    }
}

/// Given a given path, find the nearest parent directory containing a
/// `gleam.toml` file.
///
/// The file must be in either the `src` or `test` directory if it is not a
/// `.gleam` file.
fn find_gleam_project_parent<IO>(io: &IO, path: &Path) -> Option<PathBuf>
where
    IO: FileSystemReader,
{
    let is_module = path.extension().map(|x| x == "gleam").unwrap_or(false);
    let mut directory = path.to_path_buf();

    while let Some(root) = directory.parent() {
        // If there's no gleam.toml in the root then we continue to the next parent.
        if !io.is_file(&root.join("gleam.toml")) {
            _ = directory.pop();
            continue;
        }

        // If it is a Gleam module then it must reside in the src or test directory.
        if is_module && !(directory.ends_with("test") || directory.ends_with("src")) {
            _ = directory.pop();
            continue;
        }

        return Some(root.to_path_buf());
    }
    None
}

#[derive(Debug)]
pub struct Project<A, B> {
    pub engine: LanguageServerEngine<A, B>,
    pub feedback: FeedbackBookKeeper,
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

    // https://github.com/gleam-lang/gleam/issues/2121
    #[test]
    fn module_in_project_but_not_src_or_test() {
        let io = InMemoryFileSystem::new();
        io.write(Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Path::new("/app/other/one/two/three.gleam")),
            None,
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
