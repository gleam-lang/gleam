use crate::{
    build::{
        dep_tree, package_compiler, package_compiler::PackageCompiler, telemetry::Telemetry, Mode,
        Module, Origin, Package, Target,
    },
    codegen::{self, ErlangApp},
    config::PackageConfig,
    io::{FileSystemIO, FileSystemWriter},
    metadata, paths, type_, warning, Error, Result, Warning,
};
use std::{
    collections::HashMap,
    fmt::Write,
    io::BufReader,
    path::{Path, PathBuf},
    time::Instant,
};

#[derive(Debug)]
pub struct ProjectCompiler<IO> {
    root_config: PackageConfig,
    configs: HashMap<String, PackageConfig>,
    importable_modules: HashMap<String, type_::Module>,
    defined_modules: HashMap<String, PathBuf>,
    warnings: Vec<Warning>,
    telemetry: Box<dyn Telemetry>,
    io: IO,
}

// TODO: test top level package has test modules compiled
// TODO: test that tests cannot be imported into src
// TODO: test that dep cycles are not allowed between packages

impl<IO> ProjectCompiler<IO>
where
    IO: FileSystemIO + Clone,
{
    pub fn new(
        root_config: PackageConfig,
        configs: HashMap<String, PackageConfig>,
        telemetry: Box<dyn Telemetry>,
        io: IO,
    ) -> Self {
        let estimated_number_of_modules = configs.len() * 5;
        Self {
            importable_modules: HashMap::with_capacity(estimated_number_of_modules),
            defined_modules: HashMap::with_capacity(estimated_number_of_modules),
            warnings: Vec::new(),
            root_config,
            telemetry,
            configs,
            io,
        }
    }

    pub fn compile(mut self) -> Result<()> {
        // Determine package processing order
        let sequence = order_packages(&self.configs)?;

        // Read and type check deps packages
        for name in sequence {
            let config = self.configs.remove(&name).expect("Missing package config");
            self.load_cache_or_compile_package(name, config)?;
        }

        // Read and type check top level package
        let root_config = std::mem::replace(&mut self.root_config, Default::default());
        let name = root_config.name.clone();
        self.compile_package(&name, root_config, SourceLocations::SrcAndTest)?;

        Ok(())
    }

    fn load_cache_or_compile_package(
        &mut self,
        name: String,
        config: PackageConfig,
    ) -> Result<(), Error> {
        let build_path = paths::build_package(Mode::Dev, Target::Erlang, &name);
        if self.io.is_directory(&build_path) {
            tracing::info!(package=%name, "Loading precompiled package");
            self.load_cached_package(build_path, &name, config)
        } else {
            self.compile_package(&name, config, SourceLocations::Src)
        }
    }

    fn load_cached_package(
        &mut self,
        build_dir: PathBuf,
        name: &str,
        config: PackageConfig,
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

    fn compile_package(
        &mut self,
        name: &str,
        config: PackageConfig,
        locations: SourceLocations,
    ) -> Result<(), Error> {
        let out_path = paths::build_package(Mode::Dev, Target::Erlang, name);
        let src_path = paths::build_deps_package_src(name);
        let test_path = paths::build_deps_package_test(name);

        self.telemetry.compiling_package(&name);
        let test_path = match locations {
            SourceLocations::SrcAndTest => Some(paths::build_deps_package_test(name)),
            _ => None,
        };

        let options = package_compiler::Options {
            target: Target::Erlang,
            src_path: src_path.clone(),
            out_path: out_path.clone(),
            test_path: test_path.clone(),
            name: name.to_string(),
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
            &config,
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
        if locations == SourceLocations::SrcAndTest {
            let name = "gleam@@main.erl";
            self.io
                .writer(&out_path.join(name))?
                .write(std::include_bytes!("../../templates/gleam@@main.erl"))?;
            modules.push(PathBuf::from(name));
        }

        // Copy across any Erlang files from src and test
        self.copy_project_erlang_files(src_path, &mut modules, &out_path, locations, test_path)?;

        // Compile Erlang to .beam files
        self.compile_erlang_to_beam(&out_path, &modules)?;

        Ok(())
    }

    fn copy_project_erlang_files(
        &mut self,
        src_path: PathBuf,
        modules: &mut Vec<PathBuf>,
        out_path: &PathBuf,
        locations: SourceLocations,
        test_path: Option<PathBuf>,
    ) -> Result<(), Error> {
        tracing::info!("copying_erlang_source_files");
        self.copy_erlang_files(&src_path, modules, out_path, locations)?;
        Ok(if let Some(test_path) = test_path {
            self.copy_erlang_files(&test_path, modules, out_path, locations)?;
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

    fn copy_erlang_files(
        &self,
        src_path: &Path,
        to_compile_modules: &mut Vec<PathBuf>,
        out_path: &Path,
        locations: SourceLocations,
    ) -> Result<()> {
        for entry in self.io.read_dir(src_path)? {
            let full_path = entry.expect("copy_erlang_files dir_entry").path();

            let extension = full_path
                .extension()
                .unwrap_or_default()
                .to_str()
                .unwrap_or_default();

            // TODO: ensure modules are not overwritten

            // Copy any Erlang modules or header files
            if extension == "erl" || extension == "hrl" {
                let relative_path = full_path
                    .strip_prefix(src_path)
                    .expect("copy_erlang_files strip prefix")
                    .to_path_buf();
                let destination = out_path.join(&relative_path);
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

fn order_packages(configs: &HashMap<String, PackageConfig>) -> Result<Vec<String>, Error> {
    dep_tree::toposort_deps(configs.values().map(package_deps_for_graph).collect())
        .map_err(convert_deps_tree_error)
}

fn convert_deps_tree_error(e: dep_tree::Error) -> Error {
    match e {
        dep_tree::Error::Cycle(packages) => Error::PackageCycle { packages },
    }
}

fn package_deps_for_graph(config: &PackageConfig) -> (String, Vec<String>) {
    let name = config.name.to_string();
    let deps: Vec<_> = config
        .dependencies
        .iter()
        .map(|(dep, _)| dep.to_string())
        .collect();
    (name, deps)
}
