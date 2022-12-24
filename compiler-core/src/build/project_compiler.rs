use crate::{
    build::{
        dep_tree, package_compiler, package_compiler::PackageCompiler, project_compiler,
        telemetry::Telemetry, Mode, Module, Origin, Package, Target,
    },
    codegen::{self, ErlangApp},
    config::PackageConfig,
    error::{FileIoAction, FileKind},
    io::{CommandExecutor, FileSystemIO, FileSystemWriter, Stdio},
    manifest::ManifestPackage,
    metadata, paths, type_,
    uid::UniqueIdGenerator,
    version::COMPILER_VERSION,
    warning, Error, Result, Warning,
};
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    io::BufReader,
    path::{Path, PathBuf},
    time::Instant,
};

use super::ErlangAppCodegenConfiguration;

// On Windows we have to call rebar3 via a little wrapper script.
//
#[cfg(not(target_os = "windows"))]
const REBAR_EXECUTABLE: &str = "rebar3";
#[cfg(target_os = "windows")]
const REBAR_EXECUTABLE: &str = "rebar3.cmd";

#[cfg(not(target_os = "windows"))]
const ELIXIR_EXECUTABLE: &str = "elixir";
#[cfg(target_os = "windows")]
const ELIXIR_EXECUTABLE: &str = "elixir.bat";

#[derive(Debug)]
pub struct Options {
    pub mode: Mode,
    pub target: Option<Target>,
    /// Whether to perform codegen for the root project. Dependencies always
    /// have codegen run. Use for the `gleam check` command.
    /// If future when we have per-module incremental builds we will need to
    /// track both whether type metadata has been produced and also whether
    /// codegen has been performed. As such there will be 2 kinds of caching.
    pub perform_codegen: bool,
}

#[derive(Debug)]
pub struct ProjectCompiler<IO> {
    // The gleam.toml config for the root package of the project
    config: PackageConfig,
    packages: HashMap<String, ManifestPackage>,
    importable_modules: im::HashMap<String, type_::Module>,
    defined_modules: im::HashMap<String, PathBuf>,
    warnings: Vec<Warning>,
    telemetry: Box<dyn Telemetry>,
    options: Options,
    ids: UniqueIdGenerator,
    io: IO,
    build_journal: HashSet<PathBuf>,
    /// We may want to silence subprocess stdout if we are running in LSP mode.
    /// The language server talks over stdio so printing would break that.
    pub subprocess_stdio: Stdio,
}

// TODO: test that tests cannot be imported into src
// TODO: test that dep cycles are not allowed between packages

impl<IO> ProjectCompiler<IO>
where
    IO: CommandExecutor + FileSystemIO + Clone,
{
    pub fn new(
        config: PackageConfig,
        options: Options,
        packages: Vec<ManifestPackage>,
        telemetry: Box<dyn Telemetry>,
        io: IO,
    ) -> Self {
        let packages = packages
            .into_iter()
            .map(|p| (p.name.to_string(), p))
            .collect();

        Self {
            importable_modules: im::HashMap::new(),
            defined_modules: im::HashMap::new(),
            ids: UniqueIdGenerator::new(),
            warnings: Vec::new(),
            subprocess_stdio: Stdio::Inherit,
            telemetry,
            packages,
            options,
            config,
            io,
            build_journal: HashSet::new(),
        }
    }

    pub fn get_importable_modules(&self) -> &im::HashMap<String, type_::Module> {
        &self.importable_modules
    }

    // TODO: test
    pub fn checkpoint(&self) -> CheckpointState {
        CheckpointState {
            importable_modules: self.importable_modules.clone(),
            defined_modules: self.defined_modules.clone(),
            ids: self.ids.fork(),
        }
    }

    // TODO: test
    pub fn restore(&mut self, checkpoint: CheckpointState) {
        self.importable_modules = checkpoint.importable_modules;
        self.defined_modules = checkpoint.defined_modules;
        self.ids = checkpoint.ids;
    }

    pub fn mode(&self) -> Mode {
        self.options.mode
    }

    pub fn take_warnings(&mut self) -> Vec<Warning> {
        std::mem::take(&mut self.warnings)
    }

    pub fn target(&self) -> Target {
        self.options.target.unwrap_or(self.config.target)
    }

    /// Returns the compiled information from the root package
    pub fn compile(&mut self) -> Result<Package> {
        self.check_gleam_version()?;
        self.compile_dependencies()?;

        if self.options.perform_codegen {
            self.telemetry.compiling_package(&self.config.name);
        } else {
            self.telemetry.checking_package(&self.config.name);
        }
        let result = self.compile_root_package();

        self.check_build_journal()?;

        // Print warnings
        for warning in &self.warnings {
            self.telemetry.warning(warning);
        }

        result
    }

    pub fn compile_root_package(&mut self) -> Result<Package, Error> {
        let config = self.config.clone();
        let modules = self.compile_gleam_package(&config, true, paths::root())?;

        Ok(Package { config, modules })
    }

    /// Checks that version file found in the build directory matches the
    /// current version of gleam. If not, we will clear the build directory
    /// before continuing. This will ensure that upgrading gleam will not leave
    /// one with confusing or hard to debug states.
    pub fn check_gleam_version(&self) -> Result<(), Error> {
        let build_path = paths::build_packages(self.mode(), self.target());
        let version_path = paths::build_gleam_version(self.mode(), self.target());
        if self.io.is_file(&version_path) {
            let version = self.io.read(&version_path)?;
            if version == COMPILER_VERSION {
                return Ok(());
            }
        }

        // Either file is missing our the versions do not match. Time to rebuild
        tracing::info!("removing_build_state_from_different_gleam_version");
        self.io.delete(&build_path)?;

        // Recreate build directory with new updated version file
        self.io.mkdir(&build_path)?;
        self.io
            .write(&version_path, COMPILER_VERSION)
            .map_err(|e| Error::FileIo {
                action: FileIoAction::WriteTo,
                kind: FileKind::File,
                path: version_path,
                err: Some(e.to_string()),
            })
    }

    /// Checks that build journal file found in the build directory matches the
    /// current build of gleam. If not, we will clear the outdated files
    pub fn check_build_journal(&self) -> Result<(), Error> {
        let build_path = paths::build_packages(self.mode(), self.target());
        let journal_path = paths::build_journal(self.mode(), self.target());
        if self.io.is_file(&journal_path) {
            let io_journals = self.io.read(&journal_path)?;
            let old_journals: HashSet<PathBuf> = io_journals.lines().map(PathBuf::from).collect();

            tracing::info!("Deleting outdated build files");
            for diff in old_journals.difference(&self.build_journal) {
                self.io.delete_file(Path::new(&diff));
            }
        }

        let contents = self
            .build_journal
            .iter()
            .map(|b| b.to_string_lossy().to_string())
            .join("\n");
        self.io
            .write(&journal_path, &contents)
            .map_err(|e| Error::FileIo {
                action: FileIoAction::WriteTo,
                kind: FileKind::File,
                path: journal_path,
                err: Some(e.to_string()),
            })
    }

    pub fn compile_dependencies(&mut self) -> Result<(), Error> {
        let sequence = order_packages(&self.packages)?;

        for name in sequence {
            let package = self.packages.remove(&name).expect("Missing package config");
            self.load_cache_or_compile_package(&package)?;
        }

        Ok(())
    }

    fn load_cache_or_compile_package(&mut self, package: &ManifestPackage) -> Result<(), Error> {
        let build_path = paths::build_package(self.mode(), self.target(), &package.name);
        if self.io.is_directory(&build_path) {
            tracing::info!(package=%package.name, "loading_precompiled_package");
            return self.load_cached_package(build_path, package);
        }

        self.telemetry.compiling_package(&package.name);
        let result = match usable_build_tool(package)? {
            BuildTool::Gleam => self.compile_gleam_dep_package(package),
            BuildTool::Rebar3 => self.compile_rebar3_dep_package(package),
            BuildTool::Mix => self.compile_mix_dep_package(package),
        };

        // TODO: test. This one is not covered by the integration tests.
        if result.is_err() {
            tracing::debug!(package=%package.name, "removing_failed_build");
            let dir = paths::build_package(self.mode(), self.target(), &package.name);
            self.io.delete(&dir)?;
        }

        result
    }

    fn compile_rebar3_dep_package(&mut self, package: &ManifestPackage) -> Result<(), Error> {
        let name = &package.name;
        let mode = self.mode();
        let target = self.target();

        let project_dir = paths::build_deps_package(name);
        let up = paths::unnest(&project_dir);
        let rebar3_path = |path: &Path| up.join(path).to_str().unwrap_or_default().to_string();
        let ebins = paths::build_packages_ebins_glob(mode, target);
        let erl_libs = paths::build_packages_erl_libs_glob(mode, target);
        let dest = paths::build_package(mode, target, name);

        // rebar3 would make this if it didn't exist, but we make it anyway as
        // we may need to copy the include, priv, and/or ebin directory into there
        self.io.mkdir(&dest)?;

        // TODO: unit test
        let src_include = project_dir.join("include");
        if self.io.is_directory(&src_include) {
            tracing::debug!("copying_include_to_build");
            // TODO: This could be a symlink
            self.io.copy_dir(&src_include, &dest)?;
        }

        // TODO: unit test
        let priv_dir = project_dir.join("priv");
        if self.io.is_directory(&priv_dir) {
            tracing::debug!("copying_priv_to_build");
            // TODO: This could be a symlink
            self.io.copy_dir(&priv_dir, &dest)?;
        }

        // TODO: unit test
        let ebin_dir = project_dir.join("ebin");
        if self.io.is_directory(&ebin_dir) {
            tracing::debug!("copying_ebin_to_build");
            // TODO: This could be a symlink
            self.io.copy_dir(&ebin_dir, &dest)?;
        }

        // TODO: test
        if target != Target::Erlang {
            tracing::info!("skipping_rebar3_build_for_non_erlang_target");
            return Ok(());
        }

        let env = [
            ("ERL_LIBS", rebar3_path(&erl_libs)),
            ("REBAR_BARE_COMPILER_OUTPUT_DIR", rebar3_path(&dest)),
            ("REBAR_PROFILE", "prod".into()),
            ("TERM", "dumb".into()),
        ];
        let args = [
            "bare".into(),
            "compile".into(),
            "--paths".into(),
            rebar3_path(&ebins),
        ];
        let status = self.io.exec(
            REBAR_EXECUTABLE,
            &args,
            &env,
            Some(&project_dir),
            self.subprocess_stdio,
        )?;

        if status == 0 {
            Ok(())
        } else {
            Err(Error::ShellCommand {
                program: "rebar3".to_string(),
                err: None,
            })
        }
    }

    fn compile_mix_dep_package(&mut self, package: &ManifestPackage) -> Result<(), Error> {
        let name = &package.name;
        let mode = self.mode();
        let target = self.target();
        let mix_target = "prod";

        // TODO: test
        if target != Target::Erlang {
            tracing::info!("skipping_mix_build_for_non_erlang_target");
            return Ok(());
        }

        let build_dir = paths::build_packages(mode, target);
        let project_dir = paths::build_deps_package(name);
        let mix_build_dir = project_dir.join("_build").join(mix_target);
        // Absolute build path is needed for mix to make accurate symlinks
        let mix_build_path = self
            .io
            .current_dir()
            .expect("Project root")
            .join(&mix_build_dir);
        let mix_build_lib_dir = mix_build_dir.join("lib");
        let up = paths::unnest(&project_dir);
        let mix_path = |path: &Path| up.join(path).to_str().unwrap_or_default().to_string();
        let ebins = paths::build_packages_ebins_glob(mode, target);
        let dest = paths::build_package(mode, target, name);

        // Elixir core libs must be loaded
        package_compiler::maybe_link_elixir_libs(&self.io, &build_dir, self.subprocess_stdio)?;

        // Prevent Mix.Compilers.ApplicationTracer warnings
        // mix would make this if it didn't exist, but we make it anyway as
        // we need to link the compiled dependencies into there
        self.io.mkdir(&mix_build_lib_dir)?;
        let deps = &package.requirements;
        for dep in deps {
            // TODO: unit test
            let dep_source = build_dir.join(&dep);
            let dep_dest = mix_build_lib_dir.join(&dep);
            if self.io.is_directory(&dep_source) && !self.io.is_directory(&dep_dest) {
                tracing::debug!("linking_{}_to_build", dep);
                self.io.symlink_dir(&dep_source, &dep_dest)?;
            }
        }

        let env = [
            ("MIX_BUILD_PATH", mix_path(&mix_build_path)),
            ("MIX_ENV", mix_target.into()),
            ("MIX_QUIET", "1".into()),
            ("TERM", "dumb".into()),
        ];
        let args = [
            "-pa".into(),
            mix_path(&ebins),
            "-S".into(),
            "mix".into(),
            "compile".into(),
            "--no-deps-check".into(),
            "--no-load-deps".into(),
            "--no-protocol-consolidation".into(),
        ];
        let status = self.io.exec(
            ELIXIR_EXECUTABLE,
            &args,
            &env,
            Some(&project_dir),
            self.subprocess_stdio,
        )?;

        if status == 0 {
            // TODO: unit test
            let source = mix_build_dir.join("lib").join(name);
            if self.io.is_directory(&source) && !self.io.is_directory(&dest) {
                tracing::debug!("linking_{}_to_build", name);
                self.io.symlink_dir(&source, &dest)?;
            }
            Ok(())
        } else {
            Err(Error::ShellCommand {
                program: "mix".to_string(),
                err: None,
            })
        }
    }

    fn compile_gleam_dep_package(&mut self, package: &ManifestPackage) -> Result<(), Error> {
        let config_path = paths::build_deps_package_config(&package.name);
        let config = PackageConfig::read(config_path, &self.io)?;
        let root = paths::build_deps_package(&package.name);
        self.compile_gleam_package(&config, false, root)
            .map(|_| ())?;
        Ok(())
    }

    fn load_cached_package(
        &mut self,
        build_dir: PathBuf,
        package: &ManifestPackage,
    ) -> Result<(), Error> {
        for path in self.io.gleam_metadata_files(&build_dir) {
            let reader = BufReader::new(self.io.reader(&path)?);
            let module = metadata::ModuleDecoder::new(self.ids.clone()).read(reader)?;
            let _ = self
                .importable_modules
                .insert(module.name.join("/"), module)
                .ok_or(())
                .expect_err("Metadata loaded for already loaded module");
        }
        Ok(())
    }

    fn compile_gleam_package(
        &mut self,
        config: &PackageConfig,
        is_root: bool,
        root_path: PathBuf,
    ) -> Result<Vec<Module>, Error> {
        let out_path = paths::build_package(self.mode(), self.target(), &config.name);
        let lib_path = paths::build_packages(self.mode(), self.target());
        let mode = self.mode();
        let target = match self.target() {
            Target::Erlang => super::TargetCodegenConfiguration::Erlang {
                app_file: Some(ErlangAppCodegenConfiguration {
                    include_dev_deps: is_root,
                }),
            },
            Target::JavaScript => super::TargetCodegenConfiguration::JavaScript {
                emit_typescript_definitions: self.config.javascript.typescript_declarations,
            },
        };
        let mut compiler = PackageCompiler::new(
            config,
            mode,
            &root_path,
            &out_path,
            &lib_path,
            &target,
            self.ids.clone(),
            self.io.clone(),
            if (is_root) {
                Some(&mut self.build_journal)
            } else {
                None
            },
        );
        compiler.write_metadata = true;
        compiler.write_entrypoint = is_root;
        compiler.compile_beam_bytecode = !is_root || self.options.perform_codegen;
        compiler.subprocess_stdio = self.subprocess_stdio;

        let mut build_manifest = compiler.load_previus_build_manifest(&mut self.importable_modules);

        // Compile project to Erlang or JavaScript source code
        let compiled = compiler.compile(
            &mut self.warnings,
            &mut self.importable_modules,
            &mut self.defined_modules,
        )?;

        compiler.update_build_manifeest(&mut build_manifest, &compiled);

        Ok(compiled)
    }
}

fn order_packages(packages: &HashMap<String, ManifestPackage>) -> Result<Vec<String>, Error> {
    dep_tree::toposort_deps(
        packages
            .values()
            .map(|package| (package.name.clone(), package.requirements.clone()))
            .collect(),
    )
    .map_err(convert_deps_tree_error)
}

fn convert_deps_tree_error(e: dep_tree::Error) -> Error {
    match e {
        dep_tree::Error::Cycle(packages) => Error::PackageCycle { packages },
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum BuildTool {
    Gleam,
    Rebar3,
    Mix,
}

/// Determine the build tool we should use to build this package
pub(crate) fn usable_build_tool(package: &ManifestPackage) -> Result<BuildTool, Error> {
    let mut mix_present = false;

    for tool in &package.build_tools {
        match tool.as_str() {
            "gleam" => return Ok(BuildTool::Gleam),
            "rebar3" => return Ok(BuildTool::Rebar3),
            "mix" => mix_present = true,
            _ => (),
        }
    }

    if mix_present {
        return Ok(BuildTool::Mix);
    }

    Err(Error::UnsupportedBuildTool {
        package: package.name.to_string(),
        build_tools: package.build_tools.clone(),
    })
}

pub struct CheckpointState {
    importable_modules: im::HashMap<String, type_::Module>,
    defined_modules: im::HashMap<String, PathBuf>,
    ids: UniqueIdGenerator,
}
