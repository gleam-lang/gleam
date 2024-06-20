mod action;
mod compilation;
mod completion;
mod definition;
mod hover;

use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
    time::SystemTime,
};

use ecow::EcoString;
use hexpm::version::{Range, Version};

use camino::{Utf8Path, Utf8PathBuf};
use lsp_types::{Position, TextDocumentIdentifier, TextDocumentPositionParams, Url};

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

pub const LSP_TEST_ROOT_PACKAGE_NAME: &str = "app";

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

    fn exists(&self, path: &Utf8Path) -> bool {
        self.io.exists(path)
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
    toml_path: Utf8PathBuf,
    package: ManifestPackage,
) {
    let compiler = &mut engine.compiler.project_compiler;
    _ = compiler.config.dependencies.insert(
        package.name.clone(),
        match package.source {
            ManifestPackageSource::Hex { .. } => Requirement::Hex {
                version: Range::new("1.0.0".into()),
            },
            ManifestPackageSource::Local { ref path } => Requirement::Path { path: path.into() },
            ManifestPackageSource::Git { ref repo, .. } => Requirement::Git { git: repo.clone() },
        },
    );
    write_toml_from_manifest(engine, toml_path, package);
}

fn add_dev_package_from_manifest<B>(
    engine: &mut LanguageServerEngine<LanguageServerTestIO, B>,
    toml_path: Utf8PathBuf,
    package: ManifestPackage,
) {
    let compiler = &mut engine.compiler.project_compiler;
    _ = compiler.config.dev_dependencies.insert(
        package.name.clone(),
        match package.source {
            ManifestPackageSource::Hex { .. } => Requirement::Hex {
                version: Range::new("1.0.0".into()),
            },
            ManifestPackageSource::Local { ref path } => Requirement::Path { path: path.into() },
            ManifestPackageSource::Git { ref repo, .. } => Requirement::Git { git: repo.clone() },
        },
    );
    write_toml_from_manifest(engine, toml_path, package);
}

fn write_toml_from_manifest<B>(
    engine: &mut LanguageServerEngine<LanguageServerTestIO, B>,
    toml_path: Utf8PathBuf,
    package: ManifestPackage,
) {
    let compiler = &mut engine.compiler.project_compiler;
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
    add_package_from_manifest(
        engine,
        path.join("gleam.toml"),
        ManifestPackage {
            name: name.into(),
            version: Version::new(1, 0, 0),
            build_tools: vec!["gleam".into()],
            otp_app: None,
            requirements: vec![],
            source: ManifestPackageSource::Local { path: path.clone() },
        },
    )
}

fn setup_engine(
    io: &LanguageServerTestIO,
) -> LanguageServerEngine<LanguageServerTestIO, LanguageServerTestIO> {
    let mut config = PackageConfig::default();
    config.name = LSP_TEST_ROOT_PACKAGE_NAME.into();
    LanguageServerEngine::new(
        config,
        io.clone(),
        FileSystemProxy::new(io.clone()),
        io.paths.clone(),
    )
    .unwrap()
}

struct TestProject<'a> {
    src: &'a str,
    root_package_modules: Vec<(&'a str, &'a str)>,
    dependency_modules: Vec<(&'a str, &'a str)>,
    test_modules: Vec<(&'a str, &'a str)>,
    hex_modules: Vec<(&'a str, &'a str)>,
    dev_hex_modules: Vec<(&'a str, &'a str)>,
    indirect_hex_modules: Vec<(&'a str, &'a str)>,
}

impl<'a> TestProject<'a> {
    pub fn for_source(src: &'a str) -> Self {
        TestProject {
            src,
            root_package_modules: vec![],
            dependency_modules: vec![],
            test_modules: vec![],
            hex_modules: vec![],
            dev_hex_modules: vec![],
            indirect_hex_modules: vec![],
        }
    }

    pub fn add_module(mut self, name: &'a str, src: &'a str) -> Self {
        self.root_package_modules.push((name, src));
        self
    }

    pub fn add_dep_module(mut self, name: &'a str, src: &'a str) -> Self {
        self.dependency_modules.push((name, src));
        self
    }

    pub fn add_test_module(mut self, name: &'a str, src: &'a str) -> Self {
        self.test_modules.push((name, src));
        self
    }

    pub fn add_hex_module(mut self, name: &'a str, src: &'a str) -> Self {
        self.hex_modules.push((name, src));
        self
    }

    pub fn add_dev_hex_module(mut self, name: &'a str, src: &'a str) -> Self {
        self.dev_hex_modules.push((name, src));
        self
    }

    pub fn add_indirect_hex_module(mut self, name: &'a str, src: &'a str) -> Self {
        self.indirect_hex_modules.push((name, src));
        self
    }

    pub fn build_engine(
        &self,
        io: &mut LanguageServerTestIO,
    ) -> LanguageServerEngine<LanguageServerTestIO, LanguageServerTestIO> {
        io.add_hex_package("hex");
        self.hex_modules.iter().for_each(|(name, code)| {
            _ = io.hex_dep_module("hex", name, code);
        });

        self.dev_hex_modules.iter().for_each(|(name, code)| {
            _ = io.hex_dep_module("dev_hex", name, code);
        });

        self.indirect_hex_modules.iter().for_each(|(name, code)| {
            _ = io.hex_dep_module("indirect_hex", name, code);
        });

        let mut engine = setup_engine(io);

        // Add an external dependency and all its modules
        add_path_dep(&mut engine, "dep");
        self.dependency_modules.iter().for_each(|(name, code)| {
            let _ = io.path_dep_module("dep", name, code);
        });

        // Add all the modules belonging to the root package
        self.root_package_modules.iter().for_each(|(name, code)| {
            let _ = io.src_module(name, code);
        });

        // Add all the test modules
        self.test_modules.iter().for_each(|(name, code)| {
            let _ = io.test_module(name, code);
        });
        for package in &io.manifest.packages {
            let toml_path = engine.paths.build_packages_package_config(&package.name);
            add_package_from_manifest(&mut engine, toml_path, package.clone());
        }

        // Add an indirect dependency manifest
        let toml_path = engine.paths.build_packages_package_config("indirect_hex");
        write_toml_from_manifest(
            &mut engine,
            toml_path,
            ManifestPackage {
                name: "indirect_hex".into(),
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![]),
                },
                build_tools: vec!["gleam".into()],
                ..Default::default()
            },
        );

        // Add a dev dependency
        let toml_path = engine.paths.build_packages_package_config("dev_hex");
        add_dev_package_from_manifest(
            &mut engine,
            toml_path,
            ManifestPackage {
                name: "dev_hex".into(),
                source: ManifestPackageSource::Hex {
                    outer_checksum: Base16Checksum(vec![]),
                },
                build_tools: vec!["gleam".into()],
                ..Default::default()
            },
        );

        engine
    }

    pub fn build_path(&self, position: Position) -> TextDocumentPositionParams {
        let path = Utf8PathBuf::from(if cfg!(target_family = "windows") {
            r"\\?\C:\src\app.gleam"
        } else {
            "/src/app.gleam"
        });

        let url = Url::from_file_path(path).unwrap();

        TextDocumentPositionParams::new(TextDocumentIdentifier::new(url), position)
    }

    pub fn build_test_path(
        &self,
        position: Position,
        test_name: &str,
    ) -> TextDocumentPositionParams {
        let path = Utf8PathBuf::from(if cfg!(target_family = "windows") {
            format!(r"\\?\C:\test\{}.gleam", test_name)
        } else {
            format!("/test/{}.gleam", test_name)
        });

        let url = Url::from_file_path(path).unwrap();

        TextDocumentPositionParams::new(TextDocumentIdentifier::new(url), position)
    }

    pub fn positioned_with_io(
        &self,
        position: Position,
    ) -> (
        LanguageServerEngine<LanguageServerTestIO, LanguageServerTestIO>,
        TextDocumentPositionParams,
    ) {
        let mut io = LanguageServerTestIO::new();
        let mut engine = self.build_engine(&mut io);

        // Add the final module we're going to be positioning the cursor in.
        _ = io.src_module("app", self.src);

        let _response = engine.compile_please();

        let param = self.build_path(position);

        (engine, param)
    }

    pub fn positioned_with_io_in_test(
        &self,
        position: Position,
        test_name: &str,
    ) -> (
        LanguageServerEngine<LanguageServerTestIO, LanguageServerTestIO>,
        TextDocumentPositionParams,
    ) {
        let mut io = LanguageServerTestIO::new();
        let mut engine = self.build_engine(&mut io);

        // Add the final module we're going to be positioning the cursor in.
        _ = io.src_module("app", self.src);

        let response = engine.compile_please();
        assert!(response.result.is_ok());

        let param = self.build_test_path(position, test_name);

        (engine, param)
    }

    pub fn at<T>(
        self,
        position: Position,
        executor: impl FnOnce(
            &mut LanguageServerEngine<LanguageServerTestIO, LanguageServerTestIO>,
            TextDocumentPositionParams,
            EcoString,
        ) -> T,
    ) -> T {
        let (mut engine, params) = self.positioned_with_io(position);

        executor(&mut engine, params, self.src.into())
    }
}
