use gleam_core::{
    Error, Result,
    build::SourceFingerprint,
    error::{FileIoAction, FileKind},
    io::{BeamCompiler, CommandExecutor, FileSystemReader, FileSystemWriter},
    paths::ProjectPaths,
};
use std::{
    collections::{HashMap, hash_map::Entry},
    time::SystemTime,
};

use camino::{Utf8Path, Utf8PathBuf};

use super::{
    DownloadDependencies, MakeLocker, engine::LanguageServerEngine, feedback::FeedbackBookKeeper,
    files::FileSystemProxy, progress::ProgressReporter,
};

/// The language server instance serves a language client, typically a text
/// editor. The editor could have multiple Gleam projects open at once, so run
/// an instance of the language server engine for each project.
///
/// This router is responsible for finding or creating an engine for a given
/// file using the nearest parent `gleam.toml` file.
///
#[derive(Debug)]
pub(crate) struct Router<IO, Reporter> {
    io: FileSystemProxy<IO>,
    engines: HashMap<Utf8PathBuf, Project<IO, Reporter>>,
    progress_reporter: Reporter,
}

impl<IO, Reporter> Router<IO, Reporter>
where
    // IO to be supplied from outside of gleam-core
    IO: FileSystemReader
        + FileSystemWriter
        + BeamCompiler
        + CommandExecutor
        + DownloadDependencies
        + MakeLocker
        + Clone,
    // IO to be supplied from inside of gleam-core
    Reporter: ProgressReporter + Clone,
{
    pub fn new(progress_reporter: Reporter, io: FileSystemProxy<IO>) -> Self {
        Self {
            io,
            engines: HashMap::new(),
            progress_reporter,
        }
    }

    pub fn project_path(&self, path: &Utf8Path) -> Option<Utf8PathBuf> {
        find_gleam_project_parent(&self.io, path)
    }

    pub fn project_for_path(
        &mut self,
        path: Utf8PathBuf,
    ) -> Result<Option<&mut Project<IO, Reporter>>> {
        // If the path is the root of a known project then return it. Otherwise
        // find the nearest parent project.
        let path = if self.engines.contains_key(&path) {
            path
        } else {
            let Some(path) = find_gleam_project_parent(&self.io, &path) else {
                return Ok(None);
            };
            path
        };

        // If the gleam.toml has changed or the build directory is missing
        // (e.g. `gleam clean`), then discard the project as the target,
        // deps, etc may have changed and we need to rebuild taking them into
        // account.
        if let Some(project) = self.engines.get(&path) {
            let paths = ProjectPaths::new(path.clone());

            if !self.io.exists(&paths.build_directory())
                || Self::gleam_toml_changed(&paths, project, &self.io)?
            {
                let _ = self.engines.remove(&path);
            }
        }

        // Look up the project, creating a new one if it does not exist.
        Ok(Some(match self.engines.entry(path.clone()) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => {
                let project =
                    Self::new_project(path, self.io.clone(), self.progress_reporter.clone())?;
                entry.insert(project)
            }
        }))
    }

    /// Has gleam.toml changed since the last time we saw this project?
    fn gleam_toml_changed(
        paths: &ProjectPaths,
        project: &Project<IO, Reporter>,
        io: &FileSystemProxy<IO>,
    ) -> Result<bool, Error> {
        // Get the location of gleam.toml for this project
        let config_path = paths.root_config();

        // See if the file modification time has changed.
        if io.modification_time(&config_path)? == project.gleam_toml_modification_time {
            return Ok(false); // Not changed
        }

        // The mtime has changed. This might not be a content change, so let's
        // check the hash.
        let toml = io.read(&config_path)?;
        let gleam_toml_changed = project.gleam_toml_fingerprint != SourceFingerprint::new(&toml);
        Ok(gleam_toml_changed)
    }

    pub fn delete_engine_for_path(&mut self, path: &Utf8Path) {
        if let Some(path) = find_gleam_project_parent(&self.io, path) {
            _ = self.engines.remove(&path);
        }
    }

    fn new_project(
        path: Utf8PathBuf,
        io: FileSystemProxy<IO>,
        progress_reporter: Reporter,
    ) -> Result<Project<IO, Reporter>, Error> {
        tracing::info!(?path, "creating_new_language_server_engine");
        let paths = ProjectPaths::new(path);
        let config_path = paths.root_config();
        let modification_time = io.modification_time(&config_path)?;
        let toml = io.read(&config_path)?;
        let config = toml::from_str(&toml).map_err(|e| Error::FileIo {
            action: FileIoAction::Parse,
            kind: FileKind::File,
            path: config_path,
            err: Some(e.to_string()),
        })?;
        let engine = LanguageServerEngine::new(config, progress_reporter, io, paths)?;
        let project = Project {
            engine,
            feedback: FeedbackBookKeeper::default(),
            gleam_toml_modification_time: modification_time,
            gleam_toml_fingerprint: SourceFingerprint::new(&toml),
        };
        Ok(project)
    }
}

/// Given a given path, find the nearest parent directory containing a
/// `gleam.toml` file.
///
/// The file must be in either the `src`, `test` or `dev` directory if it is not a
/// `.gleam` file.
fn find_gleam_project_parent<IO>(io: &IO, path: &Utf8Path) -> Option<Utf8PathBuf>
where
    IO: FileSystemReader,
{
    let is_module = path.extension().map(|x| x == "gleam").unwrap_or(false);
    let mut directory = path.to_path_buf();

    // If we are finding the gleam project of a directory then we want to check the directory itself
    let is_directory = path.extension().is_none();
    if is_directory {
        directory.push("src");
    }

    while let Some(root) = directory.parent() {
        // If there's no gleam.toml in the root then we continue to the next parent.
        if !io.is_file(&root.join("gleam.toml")) {
            _ = directory.pop();
            continue;
        }

        // If it is a Gleam module then it must reside in the src, test dev directory.
        if is_module
            && !(directory.ends_with("test")
                || directory.ends_with("src")
                || directory.ends_with("dev"))
        {
            _ = directory.pop();
            continue;
        }

        return Some(root.to_path_buf());
    }
    None
}

#[derive(Debug)]
pub(crate) struct Project<A, B> {
    pub engine: LanguageServerEngine<A, B>,
    pub feedback: FeedbackBookKeeper,
    pub gleam_toml_modification_time: SystemTime,
    pub gleam_toml_fingerprint: SourceFingerprint,
}

#[cfg(test)]
mod find_gleam_project_parent_tests {
    use super::*;
    use gleam_core::io::{FileSystemWriter, memory::InMemoryFileSystem};

    #[test]
    fn root() {
        let io = InMemoryFileSystem::new();
        assert_eq!(find_gleam_project_parent(&io, Utf8Path::new("/")), None);
    }

    #[test]
    fn outside_a_project() {
        let io = InMemoryFileSystem::new();
        assert_eq!(
            find_gleam_project_parent(&io, Utf8Path::new("/app/src/one.gleam")),
            None
        );
    }

    #[test]
    fn gleam_toml_itself() {
        let io = InMemoryFileSystem::new();
        io.write(Utf8Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Utf8Path::new("/app/gleam.toml")),
            Some(Utf8PathBuf::from("/app"))
        );
    }

    #[test]
    fn directory_with_gleam_toml() {
        let io = InMemoryFileSystem::new();
        io.write(Utf8Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Utf8Path::new("/app")),
            Some(Utf8PathBuf::from("/app"))
        );
    }

    #[test]
    fn test_module() {
        let io = InMemoryFileSystem::new();
        io.write(Utf8Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Utf8Path::new("/app/test/one/two/three.gleam")),
            Some(Utf8PathBuf::from("/app"))
        );
    }

    #[test]
    fn src_module() {
        let io = InMemoryFileSystem::new();
        io.write(Utf8Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Utf8Path::new("/app/src/one/two/three.gleam")),
            Some(Utf8PathBuf::from("/app"))
        );
    }

    #[test]
    fn dev_module() {
        let io = InMemoryFileSystem::new();
        io.write(Utf8Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Utf8Path::new("/app/dev/one/two/three.gleam")),
            Some(Utf8PathBuf::from("/app"))
        );
    }

    // https://github.com/gleam-lang/gleam/issues/2121
    #[test]
    fn module_in_project_but_not_src_or_test_or_dev() {
        let io = InMemoryFileSystem::new();
        io.write(Utf8Path::new("/app/gleam.toml"), "").unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Utf8Path::new("/app/other/one/two/three.gleam")),
            None,
        );
    }

    #[test]
    fn nested_projects() {
        let io = InMemoryFileSystem::new();
        io.write(Utf8Path::new("/app/gleam.toml"), "").unwrap();
        io.write(Utf8Path::new("/app/examples/wibble/gleam.toml"), "")
            .unwrap();
        assert_eq!(
            find_gleam_project_parent(&io, Utf8Path::new("/app/src/one.gleam")),
            Some(Utf8PathBuf::from("/app"))
        );
        assert_eq!(
            find_gleam_project_parent(&io, Utf8Path::new("/app/examples/wibble/src/one.gleam")),
            Some(Utf8PathBuf::from("/app/examples/wibble"))
        );
    }
}
