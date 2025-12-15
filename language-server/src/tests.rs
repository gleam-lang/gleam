mod action;
mod compilation;
mod completion;
mod definition;
mod document_symbols;
mod hover;
mod reference;
mod rename;
mod router;
mod signature_help;

use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, Mutex},
    time::SystemTime,
};

use ecow::EcoString;
use hexpm::version::{Range, Version};

use camino::{Utf8Path, Utf8PathBuf};
use itertools::Itertools;
use lsp_types::{Position, TextDocumentIdentifier, TextDocumentPositionParams, Url};

use gleam_core::{
    Result,
    config::PackageConfig,
    io::{
        BeamCompiler, Command, CommandExecutor, FileSystemReader, FileSystemWriter, ReadDir,
        WrappedReader, memory::InMemoryFileSystem,
    },
    line_numbers::LineNumbers,
    manifest::{Base16Checksum, Manifest, ManifestPackage, ManifestPackageSource},
    paths::ProjectPaths,
    requirement::Requirement,
};

use super::{
    DownloadDependencies, LockGuard, Locker, MakeLocker, engine::LanguageServerEngine,
    files::FileSystemProxy, progress::ProgressReporter,
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

fn default_manifest_package() -> ManifestPackage {
    ManifestPackage {
        name: Default::default(),
        build_tools: Default::default(),
        otp_app: Default::default(),
        requirements: Default::default(),
        version: Version::new(1, 0, 0),
        source: ManifestPackageSource::Hex {
            outer_checksum: Base16Checksum(vec![]),
        },
    }
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

    pub fn dev_module(&self, name: &str, code: &str) -> Utf8PathBuf {
        let dev_directory = self.paths.dev_directory();
        let path = dev_directory.join(name).with_extension("gleam");
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
            ..default_manifest_package()
        });
    }

    fn module(&self, path: &Utf8Path, code: &str) {
        self.io.write(path, code).unwrap();
        self.io
            .try_set_modification_time(path, SystemTime::now())
            .unwrap();
    }

    fn record(&self, action: Action) {
        self.actions.lock().unwrap().push(action);
    }
}

impl FileSystemReader for LanguageServerTestIO {
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

    fn canonicalise(&self, path: &Utf8Path) -> Result<Utf8PathBuf, gleam_core::Error> {
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

    fn write(&self, path: &Utf8Path, content: &str) -> Result<(), gleam_core::Error> {
        self.io.write(path, content)
    }

    fn write_bytes(&self, path: &Utf8Path, content: &[u8]) -> Result<(), gleam_core::Error> {
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
    fn exec(&self, command: Command) -> Result<i32> {
        let Command {
            program,
            args,
            env,
            cwd,
            stdio,
        } = command;
        panic!("exec({program:?}, {args:?}, {env:?}, {cwd:?}, {stdio:?}) is not implemented")
    }
}

impl BeamCompiler for LanguageServerTestIO {
    fn compile_beam(
        &self,
        out: &Utf8Path,
        lib: &Utf8Path,
        modules: &HashSet<Utf8PathBuf>,
        stdio: gleam_core::io::Stdio,
    ) -> Result<Vec<String>> {
        panic!(
            "compile_beam({:?}, {:?}, {:?}, {:?}) is not implemented",
            out, lib, modules, stdio
        )
    }
}

impl MakeLocker for LanguageServerTestIO {
    fn make_locker(
        &self,
        _paths: &ProjectPaths,
        _target: gleam_core::build::Target,
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
    fn lock_for_build(&self) -> Result<LockGuard> {
        self.record(Action::LockBuild);
        Ok(LockGuard(Box::new(Guard(self.actions.clone()))))
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
                version: Range::new("1.0.0".into()).unwrap(),
            },
            ManifestPackageSource::Local { ref path } => Requirement::Path { path: path.into() },
            ManifestPackageSource::Git {
                ref repo,
                ref commit,
            } => Requirement::Git {
                git: repo.clone(),
                ref_: commit.clone(),
            },
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
                version: Range::new("1.0.0".into()).unwrap(),
            },
            ManifestPackageSource::Local { ref path } => Requirement::Path { path: path.into() },
            ManifestPackageSource::Git {
                ref repo,
                ref commit,
            } => Requirement::Git {
                git: repo.clone(),
                ref_: commit.clone(),
            },
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
    dev_modules: Vec<(&'a str, &'a str)>,
    hex_modules: Vec<(&'a str, &'a str)>,
    dev_hex_modules: Vec<(&'a str, &'a str)>,
    indirect_hex_modules: Vec<(&'a str, &'a str)>,
    package_modules: HashMap<&'a str, Vec<(&'a str, &'a str)>>,
}

impl<'a> TestProject<'a> {
    pub fn for_source(src: &'a str) -> Self {
        TestProject {
            src,
            root_package_modules: vec![],
            dependency_modules: vec![],
            test_modules: vec![],
            dev_modules: vec![],
            hex_modules: vec![],
            dev_hex_modules: vec![],
            indirect_hex_modules: vec![],
            package_modules: HashMap::new(),
        }
    }

    pub fn module_name_from_url(&self, url: &Url) -> Option<String> {
        Some(
            url.path_segments()?
                .skip_while(|segment| *segment != "src")
                .skip(1)
                .join("/")
                .trim_end_matches(".gleam")
                .into(),
        )
    }

    pub fn src_from_module_url(&self, url: &Url) -> Option<&str> {
        let module_name: EcoString = self.module_name_from_url(url)?.into();

        if module_name == "app" {
            return Some(self.src);
        }

        let find_module = |modules: &Vec<(&'a str, &'a str)>| {
            modules
                .iter()
                .find(|(name, _)| *name == module_name)
                .map(|(_, src)| *src)
        };

        find_module(&self.root_package_modules)
            .or_else(|| find_module(&self.dependency_modules))
            .or_else(|| find_module(&self.test_modules))
            .or_else(|| find_module(&self.hex_modules))
            .or_else(|| find_module(&self.dev_hex_modules))
            .or_else(|| find_module(&self.indirect_hex_modules))
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

    pub fn add_dev_module(mut self, name: &'a str, src: &'a str) -> Self {
        self.dev_modules.push((name, src));
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

    pub fn add_package_module(mut self, package: &'a str, name: &'a str, src: &'a str) -> Self {
        self.package_modules
            .entry(package)
            .or_default()
            .push((name, src));
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

        for (package, modules) in self.package_modules.iter() {
            io.add_hex_package(package);
            for (module, code) in modules {
                _ = io.hex_dep_module(package, module, code);
            }
        }

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

        // Add all the dev modules
        self.dev_modules.iter().for_each(|(name, code)| {
            let _ = io.dev_module(name, code);
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
                ..default_manifest_package()
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
                ..default_manifest_package()
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
            format!(r"\\?\C:\test\{test_name}.gleam")
        } else {
            format!("/test/{test_name}.gleam")
        });

        let url = Url::from_file_path(path).unwrap();

        TextDocumentPositionParams::new(TextDocumentIdentifier::new(url), position)
    }

    pub fn build_dev_path(
        &self,
        position: Position,
        test_name: &str,
    ) -> TextDocumentPositionParams {
        let path = Utf8PathBuf::from(if cfg!(target_family = "windows") {
            format!(r"\\?\C:\dev\{test_name}.gleam")
        } else {
            format!("/dev/{test_name}.gleam")
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

    pub fn positioned_with_io_in_dev(
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

        let param = self.build_dev_path(position, test_name);

        (engine, param)
    }

    pub fn at<T>(
        &self,
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

#[derive(Clone)]
pub struct PositionFinder {
    value: EcoString,
    offset: usize,
    nth_occurrence: usize,
}

pub struct RangeSelector {
    from: PositionFinder,
    to: PositionFinder,
}

impl RangeSelector {
    pub fn find_range(&self, src: &str) -> lsp_types::Range {
        lsp_types::Range {
            start: self.from.find_position(src),
            end: self.to.find_position(src),
        }
    }
}

impl PositionFinder {
    pub fn with_char_offset(self, offset: usize) -> Self {
        Self {
            value: self.value,
            offset,
            nth_occurrence: self.nth_occurrence,
        }
    }

    pub fn under_char(self, char: char) -> Self {
        Self {
            offset: self.value.find(char).unwrap_or(0),
            value: self.value,
            nth_occurrence: self.nth_occurrence,
        }
    }

    pub fn under_last_char(self) -> Self {
        let len = self.value.len();
        self.with_char_offset(len - 1)
    }

    pub fn nth_occurrence(self, nth_occurrence: usize) -> Self {
        Self {
            value: self.value,
            offset: self.offset,
            nth_occurrence,
        }
    }

    pub fn for_value(value: &str) -> Self {
        Self {
            value: value.into(),
            offset: 0,
            nth_occurrence: 1,
        }
    }

    pub fn find_position(&self, src: &str) -> Position {
        let PositionFinder {
            value,
            offset,
            nth_occurrence,
        } = self;

        let byte_index = src
            .match_indices(value.as_str())
            .nth(nth_occurrence - 1)
            .expect("no match for position")
            .0;

        byte_index_to_position(src, byte_index + offset)
    }

    pub fn select_until(self, end: PositionFinder) -> RangeSelector {
        RangeSelector {
            from: self,
            to: end,
        }
    }

    pub fn to_selection(self) -> RangeSelector {
        RangeSelector {
            from: self.clone(),
            to: self,
        }
    }
}

pub fn find_position_of(value: &str) -> PositionFinder {
    PositionFinder::for_value(value)
}

fn byte_index_to_position(src: &str, byte_index: usize) -> Position {
    let mut line = 0;
    let mut col = 0;

    for (i, char) in src.bytes().enumerate() {
        if i == byte_index {
            break;
        }

        if char == b'\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    Position::new(line, col)
}

/// This function replicates how the text editor applies TextEdit.
///
pub fn apply_code_edit(src: &str, mut change: Vec<lsp_types::TextEdit>) -> String {
    let mut result = src.to_string();
    let line_numbers = LineNumbers::new(src);
    let mut offset = 0;

    change.sort_by_key(|edit| (edit.range.start.line, edit.range.start.character));
    for edit in change {
        let start = line_numbers.byte_index(edit.range.start) as i32 - offset;
        let end = line_numbers.byte_index(edit.range.end) as i32 - offset;
        let range = (start as usize)..(end as usize);
        offset += end - start;
        offset -= edit.new_text.len() as i32;
        result.replace_range(range, &edit.new_text);
    }

    result
}
