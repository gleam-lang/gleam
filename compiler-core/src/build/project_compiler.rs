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
pub struct ProjectCompiler<'a, IO> {
    config: PackageConfig,
    packages: HashMap<String, &'a ManifestPackage>,
    importable_modules: HashMap<String, type_::Module>,
    defined_modules: HashMap<String, PathBuf>,
    warnings: Vec<Warning>,
    telemetry: Box<dyn Telemetry>,
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
    pub fn compile(mut self, target: Target) -> Result<Package> {
        // Determine package processing order
        let sequence = order_packages(&self.packages)?;

        // Read and type check deps packages
        for name in sequence {
            let package = self.packages.remove(&name).expect("Missing package config");
            self.load_cache_or_compile_package(package, target)?;
        }

        // Read and type check top level package
        self.telemetry.compiling_package(&self.config.name);
        let config = std::mem::take(&mut self.config);
        self.compile_gleam_package(&config, paths::src(), Some(paths::test()), target)
    }

    fn load_cache_or_compile_package(
        &mut self,
        package: &ManifestPackage,
        target: Target,
    ) -> Result<(), Error> {
        println!("load_cache_or_compile_package");

        let build_path = paths::build_package(Mode::Dev, target, &package.name);
        if self.io.is_directory(&build_path) {
            tracing::info!(package=%package.name, "Loading precompiled package");
            return self.load_cached_package(build_path, package);
        }

        self.telemetry.compiling_package(&package.name);
        match usable_build_tool(package)? {
            BuildTool::Gleam => self.compile_gleam_dep_package(package, target)?,
            BuildTool::Rebar3 => self.compile_rebar3_dep_package(package, target)?,
        }
        Ok(())
    }

    fn compile_rebar3_dep_package(
        &mut self,
        package: &ManifestPackage,
        target: Target,
    ) -> Result<(), Error> {
        let name = &package.name;
        let mode = Mode::Dev;

        let project_dir = paths::build_deps_package(&package.name);
        let up = paths::unnest(&project_dir);
        let rebar3_path = |path: &Path| up.join(path).to_str().unwrap_or_default().to_string();
        let ebins = paths::build_packages_ebins_glob(mode, target);
        let erl_libs = paths::build_packages_erl_libs_glob(mode, target);
        let dest = paths::build_package(mode, target, name);

        // rebar3 would make this if it didn't exist, but we make it anyway as
        // we may need to copy the include directory into there
        self.io.mkdir(&dest)?;

        let src_include = project_dir.join("include");
        if self.io.is_directory(&src_include) {
            tracing::debug!("copying_include_to_build");
            // TODO: This could be a symlink
            self.io.copy_dir(&src_include, &dest)?;
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

        if status.success() {
            Ok(())
        } else {
            Err(Error::ShellCommand {
                command: "rebar3".to_string(),
                err: None,
            })
        }
    }

    fn compile_gleam_dep_package(
        &mut self,
        package: &ManifestPackage,
        target: Target,
    ) -> Result<(), Error> {
        println!("compile_gleam_dep_package");
        let config_path = paths::build_deps_package_config(&package.name);
        let config = PackageConfig::read(config_path, &self.io)?;
        let src = paths::build_deps_package_src(&package.name);
        self.compile_gleam_package(&config, src, None, target)
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
        src_path: PathBuf,
        test_path: Option<PathBuf>,
        target: Target,
    ) -> Result<Package, Error> {
        let out_path = paths::build_package(Mode::Dev, target, &config.name);

        println!("compile_gleam_package: {:?}", out_path);

        let options = package_compiler::Options {
            target: target,
            src_path: src_path.clone(),
            out_path: out_path.clone(),
            test_path: test_path.clone(),
            name: config.name.to_string(),
            write_metadata: true,
        };

        let mut compiler = options.into_compiler(self.io.clone())?;

        // Compile project to Erlang or JavaScript source code
        let compiled = compiler.compile(
            &mut self.warnings,
            &mut self.importable_modules,
            &mut self.defined_modules,
        )?;

        if (target == Target::Erlang) {
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
        }

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

    fn compile_erlang_to_beam(&self, out_path: &Path, modules: &[PathBuf]) -> Result<(), Error> {
        tracing::info!("compiling_erlang");

        let erl_libs = paths::build_packages_erl_libs_glob(Mode::Dev, Target::Erlang);
        let env = [
            ("ERL_LIBS", erl_libs.to_string_lossy().to_string()),
            ("TERM", "dumb".into()),
        ];
        let mut args = vec![
            "-o".into(),
            out_path.join("ebin").to_string_lossy().to_string(),
        ];
        for module in modules {
            args.push(out_path.join(module).to_string_lossy().to_string());
        }
        let status = self.io.exec("erlc", &args, &env, None)?;

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

    Err(Error::UnsupportedBuildTool {
        package: package.name.to_string(),
        build_tools: package.build_tools.clone(),
    })
}
