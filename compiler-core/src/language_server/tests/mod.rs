mod compilation;
mod completion;

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    time::SystemTime,
};

use crate::{
    config::PackageConfig,
    io::{
        memory::InMemoryFileSystem, CommandExecutor, FileSystemReader, FileSystemWriter, ReadDir,
        WrappedReader,
    },
    language_server::{
        engine::LanguageServerEngine, files::FileSystemProxy, progress::ProgressReporter,
        DownloadDependencies, LockGuard, Locker, MakeLocker,
    },
    manifest::Manifest,
    paths::ProjectPaths,
    Result,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Action {
    CompilationStarted,
    CompilationFinished,
    DependencyDownloadingStarted,
    DependencyDownloadingFinished,
    DownloadDependencies,
    LockBuild,
    UnlockBuild,
}

#[derive(Debug, Clone)]
struct LanguageServerTestIO {
    io: InMemoryFileSystem,
    paths: ProjectPaths,
    actions: Arc<Mutex<Vec<Action>>>,
}

impl LanguageServerTestIO {
    fn new() -> Self {
        Self {
            io: Default::default(),
            actions: Default::default(),
            paths: ProjectPaths::at_filesystem_root(),
        }
    }

    /// Panics if there are other references to the actions.
    pub fn into_actions(self) -> Vec<Action> {
        Arc::try_unwrap(self.actions).unwrap().into_inner().unwrap()
    }

    pub fn src_module(&self, name: &str, code: &str) {
        let src_dir = self.paths.src_directory();
        let path = src_dir.join(name).with_extension("gleam");
        self.io.write(&path, code).unwrap()
    }

    pub fn test_module(&self, name: &str, code: &str) {
        let test_dir = self.paths.test_directory();
        let path = test_dir.join(name).with_extension("gleam");
        self.io.write(&path, code).unwrap()
    }

    fn record(&self, action: Action) {
        self.actions.lock().unwrap().push(action);
    }
}

impl FileSystemReader for LanguageServerTestIO {
    fn gleam_source_files(&self, dir: &Path) -> Vec<PathBuf> {
        self.io.gleam_source_files(dir)
    }

    fn gleam_extension_files(&self, dir: &Path) -> Vec<PathBuf> {
        self.io.gleam_extension_files(dir)
    }

    fn gleam_cache_files(&self, dir: &Path) -> Vec<PathBuf> {
        self.io.gleam_cache_files(dir)
    }

    fn read_dir(&self, path: &Path) -> Result<ReadDir> {
        self.io.read_dir(path)
    }

    fn read(&self, path: &Path) -> Result<String> {
        self.io.read(path)
    }

    fn read_bytes(&self, path: &Path) -> Result<Vec<u8>> {
        self.io.read_bytes(path)
    }

    fn reader(&self, path: &Path) -> Result<WrappedReader> {
        self.io.reader(path)
    }

    fn is_file(&self, path: &Path) -> bool {
        self.io.is_file(path)
    }

    fn is_directory(&self, path: &Path) -> bool {
        self.io.is_directory(path)
    }

    fn modification_time(&self, path: &Path) -> Result<SystemTime> {
        self.io.modification_time(path)
    }
}

impl FileSystemWriter for LanguageServerTestIO {
    fn mkdir(&self, path: &Path) -> Result<()> {
        self.io.mkdir(path)
    }

    fn delete(&self, path: &Path) -> Result<()> {
        self.io.delete(path)
    }

    fn copy(&self, from: &Path, to: &Path) -> Result<()> {
        self.io.copy(from, to)
    }

    fn copy_dir(&self, from: &Path, to: &Path) -> Result<()> {
        self.io.copy_dir(from, to)
    }

    fn hardlink(&self, from: &Path, to: &Path) -> Result<()> {
        self.io.hardlink(from, to)
    }

    fn symlink_dir(&self, from: &Path, to: &Path) -> Result<()> {
        self.io.symlink_dir(from, to)
    }

    fn delete_file(&self, path: &Path) -> Result<()> {
        self.io.delete_file(path)
    }

    fn write(&self, path: &Path, content: &str) -> Result<(), crate::Error> {
        self.io.write(path, content)
    }

    fn write_bytes(&self, path: &Path, content: &[u8]) -> Result<(), crate::Error> {
        self.io.write_bytes(path, content)
    }
}

impl DownloadDependencies for LanguageServerTestIO {
    fn download_dependencies(&self, _paths: &ProjectPaths) -> Result<Manifest> {
        self.record(Action::DownloadDependencies);
        Ok(Manifest {
            requirements: HashMap::new(),
            packages: vec![],
        })
    }
}

impl CommandExecutor for LanguageServerTestIO {
    fn exec(
        &self,
        program: &str,
        args: &[String],
        env: &[(&str, String)],
        cwd: Option<&Path>,
        stdio: crate::io::Stdio,
    ) -> Result<i32> {
        panic!(
            "exec({:?}, {:?}, {:?}, {:?}, {:?}) is not implemented",
            program, args, env, cwd, stdio
        )
    }
}

impl MakeLocker for LanguageServerTestIO {
    fn make_locker(
        &self,
        _paths: &ProjectPaths,
        _target: crate::build::Target,
    ) -> Result<Box<dyn Locker>> {
        Ok(Box::new(TestLocker {
            actions: self.actions.clone(),
        }))
    }
}

#[derive(Debug, Clone)]
struct TestLocker {
    actions: Arc<Mutex<Vec<Action>>>,
}

impl TestLocker {
    fn record(&self, action: Action) {
        self.actions.lock().unwrap().push(action);
    }
}

impl Locker for TestLocker {
    fn lock_for_build(&self) -> LockGuard {
        self.record(Action::LockBuild);
        LockGuard(Box::new(Guard(self.actions.clone())))
    }
}

struct Guard(Arc<Mutex<Vec<Action>>>);

impl Drop for Guard {
    fn drop(&mut self) {
        self.0.lock().unwrap().push(Action::UnlockBuild);
    }
}

impl ProgressReporter for LanguageServerTestIO {
    fn compilation_started(&self) {
        self.record(Action::CompilationStarted);
    }

    fn compilation_finished(&self) {
        self.record(Action::CompilationFinished);
    }

    fn dependency_downloading_started(&self) {
        self.record(Action::DependencyDownloadingStarted);
    }

    fn dependency_downloading_finished(&self) {
        self.record(Action::DependencyDownloadingFinished);
    }
}

fn setup_engine(
    io: &LanguageServerTestIO,
) -> LanguageServerEngine<LanguageServerTestIO, LanguageServerTestIO> {
    LanguageServerEngine::new(
        PackageConfig::default(),
        io.clone(),
        FileSystemProxy::new(io.clone()),
        io.paths.clone(),
    )
    .unwrap()
}
