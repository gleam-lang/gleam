use crate::{
    Error, Result, Warning,
    analyse::TargetSupport,
    build::{
        Mode, Module, Origin, Package, Target,
        package_compiler::{self, PackageCompiler},
        package_loader::StaleTracker,
        project_compiler,
        telemetry::Telemetry,
    },
    codegen::{self, ErlangApp},
    config::{PackageConfig, PackageKind},
    dep_tree,
    error::{DefinedModuleOrigin, FileIoAction, FileKind, ShellCommandFailureReason},
    io::{BeamCompiler, Command, CommandExecutor, FileSystemReader, FileSystemWriter, Stdio},
    manifest::{ManifestPackage, ManifestPackageSource},
    metadata,
    paths::{self, ProjectPaths},
    type_::{self, ModuleFunction},
    uid::UniqueIdGenerator,
    version::COMPILER_VERSION,
    warning::{self, WarningEmitter, WarningEmitterIO},
};
use ecow::EcoString;
use hexpm::version::Version;
use itertools::Itertools;
use pubgrub::Range;
use std::{
    cmp,
    collections::{HashMap, HashSet},
    fmt::Write,
    io::BufReader,
    rc::Rc,
    sync::Arc,
    time::Instant,
};

use super::{
    Codegen, Compile, ErlangAppCodegenConfiguration, Outcome,
    elixir_libraries::ElixirLibraries,
    package_compiler::{CachedWarnings, CheckModuleConflicts, Compiled},
};

use camino::{Utf8Path, Utf8PathBuf};

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
    pub compile: Compile,
    pub codegen: Codegen,
    pub warnings_as_errors: bool,
    pub root_target_support: TargetSupport,
    pub no_print_progress: bool,
}

#[derive(Debug)]
pub struct Built {
    pub root_package: Package,
    pub module_interfaces: im::HashMap<EcoString, type_::ModuleInterface>,
    compiled_dependency_modules: Vec<Module>,
}

impl Built {
    pub fn get_main_function(
        &self,
        module: &EcoString,
        target: Target,
    ) -> Result<ModuleFunction, Error> {
        match self.module_interfaces.get(module) {
            Some(module_data) => module_data.get_main_function(target),
            None => Err(Error::ModuleDoesNotExist {
                module: module.clone(),
                suggestion: None,
            }),
        }
    }

    pub fn minimum_required_version(&self) -> Version {
        self.module_interfaces
            .values()
            .map(|interface| &interface.minimum_required_version)
            .reduce(|one_version, other_version| cmp::max(one_version, other_version))
            .map(|minimum_required_version| minimum_required_version.clone())
            .unwrap_or(Version::new(0, 1, 0))
    }
}

#[derive(Debug)]
pub struct ProjectCompiler<IO> {
    // The gleam.toml config for the root package of the project
    pub config: PackageConfig,
    pub packages: HashMap<String, ManifestPackage>,
    importable_modules: im::HashMap<EcoString, type_::ModuleInterface>,
    pub(crate) defined_modules: im::HashMap<EcoString, DefinedModuleOrigin>,
    stale_modules: StaleTracker,
    /// The set of modules that have had partial compilation done since the last
    /// successful compilation.
    incomplete_modules: HashSet<EcoString>,
    warnings: WarningEmitter,
    telemetry: &'static dyn Telemetry,
    options: Options,
    paths: ProjectPaths,
    ids: UniqueIdGenerator,
    pub io: IO,
    /// We may want to silence subprocess stdout if we are running in LSP mode.
    /// The language server talks over stdio so printing would break that.
    pub subprocess_stdio: Stdio,
}

// TODO: test that tests cannot be imported into src
// TODO: test that dep cycles are not allowed between packages

impl<IO> ProjectCompiler<IO>
where
    IO: CommandExecutor + FileSystemWriter + FileSystemReader + BeamCompiler + Clone,
{
    pub fn new(
        config: PackageConfig,
        options: Options,
        packages: Vec<ManifestPackage>,
        telemetry: &'static dyn Telemetry,
        warning_emitter: Rc<dyn WarningEmitterIO>,
        paths: ProjectPaths,
        io: IO,
    ) -> Self {
        let packages = packages
            .into_iter()
            .map(|p| (p.name.to_string(), p))
            .collect();

        Self {
            importable_modules: im::HashMap::new(),
            defined_modules: im::HashMap::new(),
            stale_modules: StaleTracker::default(),
            incomplete_modules: HashSet::new(),
            ids: UniqueIdGenerator::new(),
            warnings: WarningEmitter::new(warning_emitter),
            subprocess_stdio: Stdio::Inherit,
            telemetry,
            packages,
            options,
            config,
            paths,
            io,
        }
    }

    pub fn mode(&self) -> Mode {
        self.options.mode
    }

    pub fn target(&self) -> Target {
        self.options.target.unwrap_or(self.config.target)
    }

    pub fn reset_state_for_new_compile_run(&mut self) {
        // We make sure the stale module tracker is empty before we start, to
        // avoid mistakenly thinking a module is stale due to outdated state
        // from a previous build. A ProjectCompiler instance is re-used by the
        // LSP engine so state could be reused if we don't reset it.

        self.stale_modules.empty();

        /// We also clear the defined modules, otherwise the language server
        /// would start throwing errors for modules defined twice when compiling
        /// a second time!
        self.defined_modules.clear();
    }

    /// Compiles all packages in the project and returns the compiled
    /// information from the root package
    pub fn compile(mut self) -> Result<Built> {
        self.reset_state_for_new_compile_run();

        // Each package may specify a Gleam version that it supports, so we
        // verify that this version is appropriate.
        self.check_gleam_version()?;

        // The JavaScript target requires a prelude module to be written.
        self.write_prelude()?;

        // Dependencies are compiled first.
        let compiled_dependency_modules = self.compile_dependencies()?;

        // We reset the warning count as we don't want to fail the build if a
        // dependency has warnings, only if the root package does.
        self.warnings.reset_count();

        let root_package = self.compile_root_package().into_result()?;

        // TODO: test
        if self.options.warnings_as_errors && self.warnings.count() > 0 {
            return Err(Error::ForbiddenWarnings {
                count: self.warnings.count(),
            });
        }

        Ok(Built {
            root_package,
            module_interfaces: self.importable_modules,
            compiled_dependency_modules,
        })
    }

    pub fn compile_root_package(&mut self) -> Outcome<Package, Error> {
        let config = self.config.clone();
        self.compile_gleam_package(&config, true, self.paths.root().to_path_buf())
            .map(
                |Compiled {
                     modules,
                     cached_module_names,
                 }| Package {
                    config,
                    modules,
                    cached_module_names,
                },
            )
    }

    /// Checks that version file found in the build directory matches the
    /// current version of gleam. If not, we will clear the build directory
    /// before continuing. This will ensure that upgrading gleam will not leave
    /// one with confusing or hard to debug states.
    pub fn check_gleam_version(&self) -> Result<(), Error> {
        let build_path = self
            .paths
            .build_directory_for_target(self.mode(), self.target());
        let version_path = self.paths.build_gleam_version(self.mode(), self.target());
        if self.io.is_file(&version_path) {
            let version = self.io.read(&version_path)?;
            if version == COMPILER_VERSION {
                return Ok(());
            }
        }

        // Either file is missing our the versions do not match. Time to rebuild
        tracing::info!("removing_build_state_from_different_gleam_version");
        self.io.delete_directory(&build_path)?;

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

    pub fn compile_dependencies(&mut self) -> Result<Vec<Module>, Error> {
        assert!(
            self.stale_modules.is_empty(),
            "The project compiler stale tracker was not emptied from the previous compilation"
        );

        let sequence = order_packages(&self.packages)?;
        let mut modules = vec![];

        for name in sequence {
            let compiled = self.load_cache_or_compile_package(&name)?;
            modules.extend(compiled);
        }

        Ok(modules)
    }

    fn write_prelude(&self) -> Result<()> {
        // Only the JavaScript target has a prelude to write.
        if !self.target().is_javascript() {
            return Ok(());
        }

        let build = self
            .paths
            .build_directory_for_target(self.mode(), self.target());

        // Write the JavaScript prelude
        let path = build.join("prelude.mjs");
        if !self.io.is_file(&path) {
            self.io.write(&path, crate::javascript::PRELUDE)?;
        }

        // Write the TypeScript prelude, if asked for
        if self.config.javascript.typescript_declarations {
            let path = build.join("prelude.d.mts");
            if !self.io.is_file(&path) {
                self.io.write(&path, crate::javascript::PRELUDE_TS_DEF)?;
            }
        }

        Ok(())
    }

    fn load_cache_or_compile_package(&mut self, name: &str) -> Result<Vec<Module>, Error> {
        // TODO: We could remove this clone if we split out the compilation of
        // packages into their own classes and then only mutate self after we no
        // longer need to have the package borrowed from self.packages.
        let package = self.packages.get(name).expect("Missing package").clone();
        let result = match usable_build_tools(&package)?.as_slice() {
            &[BuildTool::Gleam] => self.compile_gleam_dep_package(&package),
            &[BuildTool::Rebar3] => self.compile_rebar3_dep_package(&package).map(|_| vec![]),
            &[BuildTool::Mix] => self.compile_mix_dep_package(&package).map(|_| vec![]),
            &[BuildTool::Mix, BuildTool::Rebar3] => self
                .compile_mix_dep_package(&package)
                .or_else(|_| self.compile_rebar3_dep_package(&package))
                .map(|_| vec![]),
            _ => {
                return Err(Error::UnsupportedBuildTool {
                    package: package.name.to_string(),
                    build_tools: package.build_tools.clone(),
                });
            }
        };

        // TODO: test. This one is not covered by the integration tests.
        if result.is_err() {
            tracing::debug!(package=%name, "removing_failed_build");
            let path = self.paths.build_directory_for_package(
                self.mode(),
                self.target(),
                package.application_name(),
            );
            self.io.delete_directory(&path)?;
        }

        result
    }

    // TODO: extract and unit test
    fn compile_rebar3_dep_package(&mut self, package: &ManifestPackage) -> Result<(), Error> {
        let application_name = package.application_name();
        let package_name = &package.name;
        let mode = self.mode();
        let target = self.target();

        let package_build = self
            .paths
            .build_directory_for_package(mode, target, application_name);

        // TODO: test
        if self.io.is_directory(&package_build) {
            tracing::debug!(%package_name, "using_precompiled_rebar3_package");
            return Ok(());
        }

        // TODO: test
        if !self.options.codegen.should_codegen(false) {
            tracing::debug!(%package_name, "skipping_rebar3_build_as_codegen_disabled");
            return Ok(());
        }

        // TODO: test
        if target != Target::Erlang {
            tracing::debug!(%package_name, "skipping_rebar3_build_for_non_erlang_target");
            return Ok(());
        }

        // Print that work is being done
        self.telemetry.compiling_package(package_name);

        let package = self.paths.build_packages_package(package_name);
        let build_packages = self.paths.build_directory_for_target(mode, target);
        let ebins = self.paths.build_packages_ebins_glob(mode, target);
        let rebar3_path = |path: &Utf8Path| format!("../{}", path);

        tracing::debug!("copying_package_to_build");
        self.io.mkdir(&package_build)?;
        self.io.copy_dir(&package, &package_build)?;

        let env = vec![
            ("ERL_LIBS".to_string(), "../*/ebin".to_string()),
            (
                "REBAR_BARE_COMPILER_OUTPUT_DIR".to_string(),
                package_build.to_string(),
            ),
            ("REBAR_PROFILE".to_string(), "prod".to_string()),
            ("REBAR_SKIP_PROJECT_PLUGINS".to_string(), "true".to_string()),
            ("TERM".to_string(), "dumb".to_string()),
        ];
        let args = vec![
            "bare".into(),
            "compile".into(),
            "--paths".into(),
            "../*/ebin".into(),
        ];

        let status = self.io.exec(Command {
            program: REBAR_EXECUTABLE.into(),
            args,
            env,
            cwd: Some(package_build),
            stdio: self.subprocess_stdio,
        })?;

        if status == 0 {
            Ok(())
        } else {
            Err(Error::ShellCommand {
                program: "rebar3".into(),
                reason: ShellCommandFailureReason::Unknown,
            })
        }
    }

    fn compile_mix_dep_package(&mut self, package: &ManifestPackage) -> Result<(), Error> {
        let application_name = package.application_name();
        let package_name = &package.name;
        let mode = self.mode();
        let target = self.target();
        let mix_target = "prod";

        let dest = self
            .paths
            .build_directory_for_package(mode, target, application_name);

        // TODO: test
        if self.io.is_directory(&dest) {
            tracing::debug!(%package_name, "using_precompiled_mix_package");
            return Ok(());
        }

        // TODO: test
        if !self.options.codegen.should_codegen(false) {
            tracing::debug!(%package_name, "skipping_mix_build_as_codegen_disabled");
            return Ok(());
        }

        // TODO: test
        if target != Target::Erlang {
            tracing::debug!(%package_name, "skipping_mix_build_for_non_erlang_target");
            return Ok(());
        }

        // Print that work is being done
        self.telemetry.compiling_package(package_name);

        let build_dir = self.paths.build_directory_for_target(mode, target);
        let project_dir = self.paths.build_packages_package(package_name);
        let mix_build_dir = project_dir.join("_build").join(mix_target);
        let mix_build_lib_dir = mix_build_dir.join("lib");
        let up = paths::unnest(&project_dir);
        let mix_path = |path: &Utf8Path| up.join(path).to_string();
        let ebins = self.paths.build_packages_ebins_glob(mode, target);

        // Elixir core libs must be loaded
        ElixirLibraries::make_available(&self.io, &build_dir, self.subprocess_stdio)?;

        // Prevent Mix.Compilers.ApplicationTracer warnings
        // mix would make this if it didn't exist, but we make it anyway as
        // we need to link the compiled dependencies into there
        self.io.mkdir(&mix_build_lib_dir)?;
        let deps = &package.requirements;
        for dep in deps {
            // TODO: unit test
            let dep_source = build_dir.join(dep.as_str());
            let dep_dest = mix_build_lib_dir.join(dep.as_str());
            if self.io.is_directory(&dep_source) && !self.io.is_directory(&dep_dest) {
                tracing::debug!("linking_{}_to_build", dep);
                self.io.symlink_dir(&dep_source, &dep_dest)?;
            }
        }

        let env = vec![
            ("MIX_BUILD_PATH".to_string(), mix_path(&mix_build_dir)),
            ("MIX_ENV".to_string(), mix_target.to_string()),
            ("MIX_QUIET".to_string(), "1".to_string()),
            ("TERM".to_string(), "dumb".to_string()),
        ];
        let args = vec![
            "-pa".to_string(),
            mix_path(&ebins),
            "-S".to_string(),
            "mix".to_string(),
            "compile".to_string(),
            "--no-deps-check".to_string(),
            "--no-load-deps".to_string(),
            "--no-protocol-consolidation".to_string(),
        ];

        let status = self.io.exec(Command {
            program: ELIXIR_EXECUTABLE.into(),
            args,
            env,
            cwd: Some(project_dir),
            stdio: self.subprocess_stdio,
        })?;

        if status == 0 {
            // TODO: unit test
            let source = mix_build_dir.join("lib").join(application_name.as_str());
            if self.io.is_directory(&source) && !self.io.is_directory(&dest) {
                tracing::debug!("linking_{}_to_build", application_name);
                self.io.symlink_dir(&source, &dest)?;
            }
            Ok(())
        } else {
            Err(Error::ShellCommand {
                program: "mix".into(),
                reason: ShellCommandFailureReason::Unknown,
            })
        }
    }

    fn compile_gleam_dep_package(
        &mut self,
        package: &ManifestPackage,
    ) -> Result<Vec<Module>, Error> {
        // TODO: Test
        let package_root = match &package.source {
            // If the path is relative it is relative to the root of the
            // project, not to the current working directory. The language server
            // could have the working directory and the project root in different
            // places.
            ManifestPackageSource::Local { path } if path.is_relative() => {
                self.io.canonicalise(&self.paths.root().join(path))?
            }

            // If the path is absolute we can use it as-is.
            ManifestPackageSource::Local { path } => path.clone(),

            // Hex and Git packages are downloaded into the project's build
            // directory.
            ManifestPackageSource::Git { .. } | ManifestPackageSource::Hex { .. } => {
                self.paths.build_packages_package(&package.name)
            }
        };
        let config_path = package_root.join("gleam.toml");
        let config = PackageConfig::read(config_path, &self.io)?;
        self.compile_gleam_package(&config, false, package_root)
            .into_result()
            .map(|compiled| compiled.modules)
    }

    fn compile_gleam_package(
        &mut self,
        config: &PackageConfig,
        is_root: bool,
        root_path: Utf8PathBuf,
    ) -> Outcome<Compiled, Error> {
        let out_path =
            self.paths
                .build_directory_for_package(self.mode(), self.target(), &config.name);
        let lib_path = self
            .paths
            .build_directory_for_target(self.mode(), self.target());
        let mode = if is_root { self.mode() } else { Mode::Prod };
        let target = match self.target() {
            Target::Erlang => {
                let package_name_overrides = self
                    .packages
                    .values()
                    .flat_map(|p| {
                        let overriden = p.otp_app.as_ref()?;
                        Some((p.name.clone(), overriden.clone()))
                    })
                    .collect();
                super::TargetCodegenConfiguration::Erlang {
                    app_file: Some(ErlangAppCodegenConfiguration {
                        include_dev_deps: is_root && self.mode().includes_dev_dependencies(),
                        package_name_overrides,
                    }),
                }
            }

            Target::JavaScript => super::TargetCodegenConfiguration::JavaScript {
                emit_typescript_definitions: self.config.javascript.typescript_declarations,
                // This path is relative to each package output directory
                prelude_location: Utf8PathBuf::from("../prelude.mjs"),
            },
        };

        let package_kind = if is_root {
            PackageKind::Root
        } else {
            PackageKind::Dependency {
                package_name: config.name.clone(),
            }
        };

        let mut compiler = PackageCompiler::new(
            package_kind,
            config,
            mode,
            &root_path,
            &out_path,
            &lib_path,
            &target,
            self.ids.clone(),
            self.io.clone(),
        );
        compiler.write_metadata = true;
        compiler.write_entrypoint = is_root;
        compiler.perform_codegen = self.options.codegen.should_codegen(is_root);
        compiler.compile_beam_bytecode = self.options.codegen.should_codegen(is_root);
        compiler.compile_modules = !(self.options.compile == Compile::DepsOnly && is_root);
        compiler.subprocess_stdio = self.subprocess_stdio;
        compiler.target_support = if is_root {
            // When compiling the root package it is context specific as to whether we need to
            // enforce that all functions have an implementation for the current target.
            // Typically we do, but if we are using `gleam run -m $module` to run a module that
            // belongs to a dependency we don't need to enforce this as we don't want to fail
            // compilation. It's impossible for a dependecy module to call functions from the root
            // package, so it's OK if they could not be compiled.
            self.options.root_target_support
        } else {
            // When compiling dependencies we don't enforce that all functions have an
            // implementation for the current target. It is OK if they have APIs that are
            // unaccessible so long as they are not used by the root package.
            TargetSupport::NotEnforced
        };
        if is_root {
            compiler.cached_warnings = CachedWarnings::Use;
            // We only check for conflicting Gleam files if this is the root
            // package, since Hex packages are bundled with the Gleam source files
            // and compiled Erlang files next to each other.
            compiler.check_module_conflicts = CheckModuleConflicts::Check;
        } else {
            compiler.cached_warnings = CachedWarnings::Ignore;
            compiler.check_module_conflicts = CheckModuleConflicts::DoNotCheck;
        };

        // Compile project to Erlang or JavaScript source code
        compiler.compile(
            &mut self.warnings,
            &mut self.importable_modules,
            &mut self.defined_modules,
            &mut self.stale_modules,
            &mut self.incomplete_modules,
            self.telemetry,
        )
    }
}

impl<IO> ProjectCompiler<IO> {
    pub fn get_importable_modules(&self) -> &im::HashMap<EcoString, type_::ModuleInterface> {
        &self.importable_modules
    }
}

fn order_packages(packages: &HashMap<String, ManifestPackage>) -> Result<Vec<EcoString>, Error> {
    dep_tree::toposort_deps(
        packages
            .values()
            // Making sure that the package order is deterministic, to prevent different
            // compilations of the same project compiling in different orders. This could impact
            // any bugged outcomes, though not any where the compiler is working correctly, so it's
            // mostly to aid debugging.
            .sorted_by(|a, b| a.name.cmp(&b.name))
            .map(|package| {
                (
                    package.name.as_str().into(),
                    package
                        .requirements
                        .iter()
                        .map(|r| EcoString::from(r.as_ref()))
                        .collect(),
                )
            })
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
pub(crate) fn usable_build_tools(package: &ManifestPackage) -> Result<Vec<BuildTool>, Error> {
    let mut rebar3_present = false;
    let mut mix_present = false;

    for tool in &package.build_tools {
        match tool.as_str() {
            "gleam" => return Ok(vec![BuildTool::Gleam]),
            "rebar" => rebar3_present = true,
            "rebar3" => rebar3_present = true,
            "mix" => mix_present = true,
            _ => (),
        }
    }

    if mix_present && rebar3_present {
        return Ok(vec![BuildTool::Mix, BuildTool::Rebar3]);
    } else if mix_present {
        return Ok(vec![BuildTool::Mix]);
    } else if rebar3_present {
        return Ok(vec![BuildTool::Rebar3]);
    }

    Err(Error::UnsupportedBuildTool {
        package: package.name.to_string(),
        build_tools: package.build_tools.clone(),
    })
}
