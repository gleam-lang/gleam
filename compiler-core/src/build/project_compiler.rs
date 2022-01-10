use crate::{
    build::{
        dep_tree, package_compiler, package_compiler::PackageCompiler, project_compiler,
        telemetry::Telemetry, Mode, Module, Origin, Package, Target,
    },
    codegen::{self, ErlangApp},
    config::PackageConfig,
    io::{CommandExecutor, FileSystemIO, FileSystemWriter},
    metadata, paths,
    project::ManifestPackage,
    type_, warning, Error, Result, Warning,
};
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    io::BufReader,
    path::{Path, PathBuf},
    time::Instant,
};

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
pub struct ProjectCompiler<'a, IO> {
    config: PackageConfig,
    packages: HashMap<String, &'a ManifestPackage>,
    importable_modules: HashMap<String, type_::Module>,
    defined_modules: HashMap<String, PathBuf>,
    warnings: Vec<Warning>,
    telemetry: Box<dyn Telemetry>,
    options: &'a Options,
    io: IO,
}

// TODO: test that tests cannot be imported into src
// TODO: test that dep cycles are not allowed between packages

impl<'a, IO> ProjectCompiler<'a, IO>
where
    IO: CommandExecutor + FileSystemIO + Clone,
{
    pub fn new(
        config: PackageConfig,
        options: &'a Options,
        packages: &'a [ManifestPackage],
        telemetry: Box<dyn Telemetry>,
        io: IO,
    ) -> Self {
        let estimated_modules = packages.len() * 5;
        let packages = packages.iter().map(|p| (p.name.to_string(), p)).collect();
        Self {
            importable_modules: HashMap::with_capacity(estimated_modules),
            defined_modules: HashMap::with_capacity(estimated_modules),
            warnings: Vec::new(),
            telemetry,
            packages,
            options,
            config,
            io,
        }
    }

    pub fn mode(&self) -> Mode {
        self.options.mode
    }

    pub fn target(&self) -> Target {
        self.options.target.unwrap_or(self.config.target)
    }

    /// Returns the compiled information from the root package
    pub fn compile(mut self) -> Result<Package> {
        // Determine package processing order
        let sequence = order_packages(&self.packages)?;

        // Read and type check deps packages
        for name in sequence {
            let package = self.packages.remove(&name).expect("Missing package config");
            self.load_cache_or_compile_package(package)?;
        }

        // Read and type check top level package
        if self.options.perform_codegen {
            self.telemetry.compiling_package(&self.config.name);
        } else {
            self.telemetry.checking_package(&self.config.name);
        }
        let config = self.config.clone();
        let modules = self.compile_gleam_package(&config, true, paths::root())?;

        // Print warnings
        for warning in self.warnings {
            self.telemetry.warning(&warning);
        }

        Ok(Package { config, modules })
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
        };

        // TODO: test. This one is not covered by the integration tests.
        if result.is_err() {
            tracing::debug!(package=%package.name,"removing_failed_build");
            let dir = paths::build_package(self.mode(), self.target(), &package.name);
            self.io.delete(&dir)?;
        }

        result
    }

    fn compile_rebar3_dep_package(&mut self, package: &ManifestPackage) -> Result<(), Error> {
        let name = &package.name;
        let mode = self.mode();
        let target = self.target();

        let project_dir = paths::build_deps_package(&package.name);
        let up = paths::unnest(&project_dir);
        let rebar3_path = |path: &Path| up.join(path).to_str().unwrap_or_default().to_string();
        let ebins = paths::build_packages_ebins_glob(mode, target);
        let erl_libs = paths::build_packages_erl_libs_glob(mode, target);
        let dest = paths::build_package(mode, target, name);

        // rebar3 would make this if it didn't exist, but we make it anyway as
        // we may need to copy the include directory into there
        self.io.mkdir(&dest)?;

        // TODO: unit test
        let src_include = project_dir.join("include");
        if self.io.is_directory(&src_include) {
            tracing::debug!("copying_include_to_build");
            // TODO: This could be a symlink
            self.io.copy_dir(&src_include, &dest)?;
        }

        // TODO: unit test
        let src_priv = project_dir.join("priv");
        if self.io.is_directory(&src_priv) {
            tracing::debug!("copying_priv_to_build");
            // TODO: This could be a symlink
            self.io.copy_dir(&src_priv, &dest)?;
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
        let status = self.io.exec("rebar3", &args, &env, Some(&project_dir))?;

        if status == 0 {
            Ok(())
        } else {
            Err(Error::ShellCommand {
                program: "rebar3".to_string(),
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
            let module = metadata::ModuleDecoder::new().read(reader)?;
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
        let artifact_path = out_path.join("build");
        let mut compiler = PackageCompiler::new(
            config,
            &root_path,
            &out_path,
            &lib_path,
            self.target(),
            self.io.clone(),
        );
        compiler.write_metadata = true;
        compiler.write_entrypoint = is_root;
        compiler.compile_beam_bytecode = !is_root || self.options.perform_codegen;
        compiler.read_source_files(self.mode())?;

        // Compile project to Erlang source code
        let compiled = compiler.compile(
            &mut self.warnings,
            &mut self.importable_modules,
            &mut self.defined_modules,
        )?;

        Ok(compiled)
    }
}

fn order_packages(packages: &HashMap<String, &ManifestPackage>) -> Result<Vec<String>, Error> {
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

#[derive(Debug)]
enum BuildTool {
    Gleam,
    Rebar3,
}

/// Determine the build tool we should use to build this package
fn usable_build_tool(package: &ManifestPackage) -> Result<BuildTool, Error> {
    for tool in &package.build_tools {
        match tool.as_str() {
            "gleam" => return Ok(BuildTool::Gleam),
            "rebar3" => return Ok(BuildTool::Rebar3),
            _ => (),
        }
    }

    Err(Error::UnsupportedBuildTool {
        package: package.name.to_string(),
        build_tools: package.build_tools.clone(),
    })
}
