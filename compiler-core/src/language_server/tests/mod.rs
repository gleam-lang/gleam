mod action;
mod compilation;
mod completion;
mod hover;

use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
    time::SystemTime,
};

use hexpm::version::Version;

use camino::{Utf8Path, Utf8PathBuf};

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
    manifest::{Base16Checksum, Manifest, ManifestPackage, ManifestPackageSource},
    paths::ProjectPaths,
    requirement::Requirement,
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
    manifest: Manifest,
}

impl LanguageServerTestIO {
    fn new() -> Self {
        Self {
            io: Default::default(),
            actions: Default::default(),
            paths: ProjectPaths::at_filesystem_root(),
            manifest: Manifest {
                requirements: HashMap::new(),
                packages: vec![],
            },
        }
    }

    /// Panics if there are other references to the actions.
    pub fn into_actions(self) -> Vec<Action> {
        Arc::try_unwrap(self.actions).unwrap().into_inner().unwrap()
    }

    pub fn src_module(&self, name: &str, code: &str) -> Utf8PathBuf {
        let src_dir = self.paths.src_directory();
        let path = src_dir.join(name).with_extension("gleam");
        self.module(&path, code);
        path
    }

    pub fn test_module(&self, name: &str, code: &str) -> Utf8PathBuf {
        let test_dir = self.paths.test_directory();
        let path = test_dir.join(name).with_extension("gleam");
        self.module(&path, code);
        path
    }

    pub fn path_dep_module(&self, dep: &str, name: &str, code: &str) -> Utf8PathBuf {
        let dep_dir = self.paths.root().join(dep).join("src");
        let path = dep_dir.join(name).with_extension("gleam");
        self.module(&path, code);
        path
    }

    pub fn hex_dep_module(&self, dep: &str, name: &str, code: &str) -> Utf8PathBuf {
        let dep_dir = self.paths.build_packages_package(dep).join("src");
        let path = dep_dir.join(name).with_extension("gleam");
        self.module(&path, code);
        path
    }

    pub fn add_hex_package(&mut self, name: &str) {
        self.manifest.packages.push(ManifestPackage {
            name: name.into(),
            source: ManifestPackageSource::Hex {
                outer_checksum: Base16Checksum(vec![]),
            },
            build_tools: vec!["gleam".into()],
            ..Default::default()
        });
    }

    fn module(&self, path: &Utf8Path, code: &str) {
        self.io.write(path, code).unwrap();
        self.io.set_modification_time(path, SystemTime::now());
    }

    fn record(&self, action: Action) {
        self.actions.lock().unwrap().push(action);
    }
}

impl FileSystemReader for LanguageServerTestIO {
    fn gleam_source_files(&self, dir: &Utf8Path) -> Vec<Utf8PathBuf> {
        self.io.gleam_source_files(dir)
    }

    fn gleam_cache_files(&self, dir: &Utf8Path) -> Vec<Utf8PathBuf> {
        self.io.gleam_cache_files(dir)
    }

    fn read_dir(&self, path: &Utf8Path) -> Result<ReadDir> {
        self.io.read_dir(path)
    }

    fn read(&self, path: &Utf8Path) -> Result<String> {
        self.io.read(path)
    }

    fn read_bytes(&self, path: &Utf8Path) -> Result<Vec<u8>> {
        self.io.read_bytes(path)
    }

    fn reader(&self, path: &Utf8Path) -> Result<WrappedReader> {
        self.io.reader(path)
    }

    fn is_file(&self, path: &Utf8Path) -> bool {
        self.io.is_file(path)
    }

    fn is_directory(&self, path: &Utf8Path) -> bool {
        self.io.is_directory(path)
    }

    fn modification_time(&self, path: &Utf8Path) -> Result<SystemTime> {
        self.io.modification_time(path)
    }

    fn canonicalise(&self, path: &Utf8Path) -> Result<Utf8PathBuf, crate::Error> {
        self.io.canonicalise(path)
    }
}

impl FileSystemWriter for LanguageServerTestIO {
    fn mkdir(&self, path: &Utf8Path) -> Result<()> {
        self.io.mkdir(path)
    }

    fn delete_directory(&self, path: &Utf8Path) -> Result<()> {
        self.io.delete_directory(path)
    }

    fn copy(&self, from: &Utf8Path, to: &Utf8Path) -> Result<()> {
        self.io.copy(from, to)
    }

    fn copy_dir(&self, from: &Utf8Path, to: &Utf8Path) -> Result<()> {
        self.io.copy_dir(from, to)
    }

    fn hardlink(&self, from: &Utf8Path, to: &Utf8Path) -> Result<()> {
        self.io.hardlink(from, to)
    }

    fn symlink_dir(&self, from: &Utf8Path, to: &Utf8Path) -> Result<()> {
        self.io.symlink_dir(from, to)
    }

    fn delete_file(&self, path: &Utf8Path) -> Result<()> {
        self.io.delete_file(path)
    }

    fn write(&self, path: &Utf8Path, content: &str) -> Result<(), crate::Error> {
        self.io.write(path, content)
    }

    fn write_bytes(&self, path: &Utf8Path, content: &[u8]) -> Result<(), crate::Error> {
        self.io.write_bytes(path, content)
    }
}

impl DownloadDependencies for LanguageServerTestIO {
    fn download_dependencies(&self, _paths: &ProjectPaths) -> Result<Manifest> {
        self.record(Action::DownloadDependencies);
        Ok(self.manifest.clone())
    }
}

impl CommandExecutor for LanguageServerTestIO {
    fn exec(
        &self,
        program: &str,
        args: &[String],
        env: &[(&str, String)],
        cwd: Option<&Utf8Path>,
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

fn add_package_from_manifest<B>(
    engine: &mut LanguageServerEngine<LanguageServerTestIO, B>,
    package: ManifestPackage,
) {
    let compiler = &mut engine.compiler.project_compiler;
    let toml_path = engine.paths.build_packages_package_config(&package.name);
    let toml = format!(
        r#"name = "{}"
    version = "{}""#,
        &package.name, &package.version
    );

    _ = compiler.packages.insert(package.name.to_string(), package);
    compiler.io.write(toml_path.as_path(), &toml).unwrap();
}

fn add_path_dep<B>(engine: &mut LanguageServerEngine<LanguageServerTestIO, B>, name: &str) {
    let path = engine.paths.root().join(name);
    let compiler = &mut engine.compiler.project_compiler;
    _ = compiler
        .config
        .dependencies
        .insert(name.into(), Requirement::Path { path: path.clone() });
    _ = compiler.packages.insert(
        name.into(),
        ManifestPackage {
            name: name.into(),
            version: Version::new(1, 0, 0),
            build_tools: vec!["gleam".into()],
            otp_app: None,
            requirements: vec![],
            source: ManifestPackageSource::Local { path: path.clone() },
        },
    );
    let toml = format!(
        r#"name = "{name}"
version = "1.0.0""#
    );
    _ = compiler.io.write(&path.join("gleam.toml"), &toml);
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
