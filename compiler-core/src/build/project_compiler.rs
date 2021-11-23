use crate::{
    build::{
        dep_tree, package_compiler, package_compiler::PackageCompiler, telemetry::Telemetry, Mode,
        Module, Origin, Package, Target,
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
pub struct ProjectCompiler<'a, IO> {
    config: PackageConfig,
    packages: HashMap<String, &'a ManifestPackage>,
    importable_modules: HashMap<String, type_::Module>,
    defined_modules: HashMap<String, PathBuf>,
    warnings: Vec<Warning>,
    telemetry: Box<dyn Telemetry>,
    io: IO,
}

// TODO: test top level package has test modules compiled
// TODO: test that tests cannot be imported into src
// TODO: test that dep cycles are not allowed between packages

impl<'a, IO> ProjectCompiler<'a, IO>
where
    IO: CommandExecutor + FileSystemIO + Clone,
{
    pub fn new(
        config: PackageConfig,
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
            config,
            telemetry,
            packages,
            io,
        }
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
        let config = std::mem::take(&mut self.config);
        self.compile_gleam_package(&config, paths::src(), Some(paths::test()))
    }

    fn load_cache_or_compile_package(&mut self, package: &ManifestPackage) -> Result<(), Error> {
        let build_path = paths::build_package(Mode::Dev, Target::Erlang, &package.name);
        if self.io.is_directory(&build_path) {
            tracing::info!(package=%package.name, "Loading precompiled package");
            return self.load_cached_package(build_path, package);
        }

        match usable_build_tool(package)? {
            BuildTool::Gleam => self.compile_gleam_dep_package(package)?,
            BuildTool::Rebar3 => self.compile_rebar3_dep_package(package)?,
        }
        Ok(())
    }

    fn compile_rebar3_dep_package(&mut self, package: &ManifestPackage) -> Result<(), Error> {
        let args = [];
        let env = [];
        // TODO: check status
        // TODO: set environment variables
        let _ = self.io.exec("rebar3", &args, &env)?;
        Ok(())
    }

    fn compile_gleam_dep_package(&mut self, package: &ManifestPackage) -> Result<(), Error> {
        let config_path = paths::build_deps_package_config(&package.name);
        let config = PackageConfig::read(config_path, &self.io)?;
        let src = paths::build_deps_package_src(&package.name);
        self.compile_gleam_package(&config, src, None).map(|_| ())?;
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
        src_path: PathBuf,
        test_path: Option<PathBuf>,
    ) -> Result<Package, Error> {
        let out_path = paths::build_package(Mode::Dev, Target::Erlang, &config.name);

        self.telemetry.compiling_package(&config.name);

        let options = package_compiler::Options {
            target: Target::Erlang,
            src_path: src_path.clone(),
            out_path: out_path.clone(),
            test_path: test_path.clone(),
            name: config.name.to_string(),
            write_metadata: true,
        };

        let mut compiler = options.into_compiler(self.io.clone())?;

        // Compile project to Erlang source code
        let compiled = compiler.compile(
            &mut self.warnings,
            &mut self.importable_modules,
            &mut self.defined_modules,
        )?;

        // Write an Erlang .app file
        ErlangApp::new(&out_path.join("ebin")).render(
            self.io.clone(),
            config,
            &compiled.modules,
        )?;

        let mut modules: Vec<_> = compiled
            .modules
            .iter()
            .map(Module::compiled_erlang_path)
            .collect();

        // If we're the dev package render the entrypoint module used by the
        // `gleam run` and `gleam test` commands
        // TODO: Pass this config in in a better way rather than attaching extra
        // meaning to this flag.
        if test_path.is_some() {
            let name = "gleam@@main.erl";
            self.io
                .writer(&out_path.join(name))?
                .write(std::include_bytes!("../../templates/gleam@@main.erl"))?;
            modules.push(PathBuf::from(name));
        }

        // Copy across any Erlang files from src and test
        self.copy_project_erlang_files(src_path, &mut modules, &out_path, test_path)?;

        // Compile Erlang to .beam files
        self.compile_erlang_to_beam(&out_path, &modules)?;

        Ok(compiled)
    }

    fn copy_project_erlang_files(
        &mut self,
        src_path: PathBuf,
        modules: &mut Vec<PathBuf>,
        out_path: &PathBuf,
        test_path: Option<PathBuf>,
    ) -> Result<(), Error> {
        tracing::info!("copying_erlang_source_files");
        let mut copied = HashSet::new();
        self.copy_erlang_files(&src_path, &mut copied, modules, out_path)?;
        Ok(if let Some(test_path) = test_path {
            self.copy_erlang_files(&test_path, &mut copied, modules, out_path)?;
        })
    }

    // TODO: remove this IO from core. Inject the command runner
    fn compile_erlang_to_beam(&self, out_path: &Path, modules: &[PathBuf]) -> Result<(), Error> {
        tracing::info!("Compiling Erlang code");
        let escript_path = paths::build_scripts().join("compile_erlang.erl");

        // Run escript to compile Erlang to beam files
        let mut command = std::process::Command::new("erlc");
        let _ = command.arg("-o");
        let _ = command.arg(out_path.join("ebin"));
        for module in modules {
            let _ = command.arg(out_path.join(module));
        }

        tracing::debug!("Running OS process {:?}", command);
        let status = command.status().map_err(|e| Error::ShellCommand {
            command: "erlc".to_string(),
            err: Some(e.kind()),
        })?;

        if status.success() {
            Ok(())
        } else {
            Err(Error::ShellCommand {
                command: "erlc".to_string(),
                err: None,
            })
        }
    }

    // TODO: test
    fn copy_erlang_files(
        &self,
        src_path: &Path,
        copied: &mut HashSet<PathBuf>,
        to_compile_modules: &mut Vec<PathBuf>,
        out_path: &Path,
    ) -> Result<()> {
        for entry in self.io.read_dir(src_path)? {
            let full_path = entry.expect("copy_erlang_files dir_entry").path();

            let extension = full_path
                .extension()
                .unwrap_or_default()
                .to_str()
                .unwrap_or_default();

            // Copy any Erlang modules or header files
            if extension == "erl" || extension == "hrl" {
                let relative_path = full_path
                    .strip_prefix(src_path)
                    .expect("copy_erlang_files strip prefix")
                    .to_path_buf();
                let destination = out_path.join(&relative_path);

                // TODO: test
                if !copied.insert(relative_path.clone()) {
                    return Err(Error::DuplicateErlangFile {
                        file: relative_path.to_string_lossy().to_string(),
                    });
                }

                self.io.copy(&full_path, &destination)?;

                // Track the new module to compile
                if extension == "erl" {
                    to_compile_modules.push(relative_path);
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum SourceLocations {
    Src,
    SrcAndTest,
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

    // TODO: return an error
    todo!()
}
