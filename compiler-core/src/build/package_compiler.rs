use crate::{
    ast::{SrcSpan, TypedModule, UntypedModule},
    build::{dep_tree, Mode, Module, Origin, Package, Target},
    codegen::{Erlang, ErlangApp, JavaScript},
    config::PackageConfig,
    error,
    io::{
        memory::InMemoryFileSystem, CommandExecutor, FileSystemIO, FileSystemReader,
        FileSystemWriter,
    },
    metadata::ModuleEncoder,
    parse::extra::ModuleExtra,
    type_,
    uid::UniqueIdGenerator,
    Error, Result, Warning,
};
use askama::Template;
use std::{collections::HashMap, fmt::write};
use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

#[derive(Debug)]
pub struct PackageCompiler<'a, IO> {
    pub io: IO,
    pub out: &'a Path,
    pub lib: &'a Path,
    pub root: &'a Path,
    pub target: Target,
    pub config: &'a PackageConfig,
    pub sources: Vec<Source>,
    pub ids: UniqueIdGenerator,
    pub write_metadata: bool,
    pub perform_codegen: bool,
    pub write_entrypoint: bool,
    pub copy_native_files: bool,
    pub compile_beam_bytecode: bool,
    pub silence_subprocess_stdout: bool,
    pub build_journal: Option<&'a mut HashSet<PathBuf>>,
}

// TODO: ensure this is not a duplicate module
// TODO: tests
// Including cases for:
// - modules that don't import anything
impl<'a, IO> PackageCompiler<'a, IO>
where
    IO: FileSystemIO + CommandExecutor + Clone,
{
    pub fn new(
        config: &'a PackageConfig,
        root: &'a Path,
        out: &'a Path,
        lib: &'a Path,
        target: Target,
        ids: UniqueIdGenerator,
        io: IO,
        build_journal: Option<&'a mut HashSet<PathBuf>>,
    ) -> Self {
        Self {
            io,
            ids,
            out,
            lib,
            root,
            config,
            target,
            sources: vec![],
            write_metadata: true,
            perform_codegen: true,
            write_entrypoint: false,
            copy_native_files: true,
            compile_beam_bytecode: true,
            silence_subprocess_stdout: false,
            build_journal,
        }
    }

    pub fn compile(
        mut self,
        warnings: &mut Vec<Warning>,
        existing_modules: &mut im::HashMap<String, type_::Module>,
        already_defined_modules: &mut im::HashMap<String, PathBuf>,
    ) -> Result<Vec<Module>, Error> {
        let span = tracing::info_span!("compile", package = %self.config.name.as_str());
        let _enter = span.enter();

        tracing::info!("Parsing source code");
        let parsed_modules = parse_sources(
            &self.config.name,
            std::mem::take(&mut self.sources),
            already_defined_modules,
        )?;

        // Determine order in which modules are to be processed
        let sequence = dep_tree::toposort_deps(
            parsed_modules
                .values()
                .map(|m| module_deps_for_graph(self.target, m))
                .collect(),
        )
        .map_err(convert_deps_tree_error)?;

        tracing::info!("Type checking modules");
        let mut modules = type_check(
            &self.config.name,
            self.target,
            &self.ids,
            sequence,
            parsed_modules,
            existing_modules,
            warnings,
        )?;

        tracing::info!("Performing code generation");
        self.perform_codegen(&modules)?;

        self.encode_and_write_metadata(&modules)?;

        Ok(modules)
    }

    fn compile_erlang_to_beam(&mut self, modules: &HashSet<PathBuf>) -> Result<(), Error> {
        tracing::info!("compiling_erlang");

        let escript_path = self.out.join("build").join("gleam@@compile.erl");
        if !escript_path.exists() {
            let escript_source = std::include_str!("../../templates/gleam@@compile.erl");
            self.io
                .writer(&escript_path)?
                .write(escript_source.as_bytes())?;
        }

        let mut args = vec![
            escript_path.to_string_lossy().to_string(),
            // Tell the compiler where to find other libraries
            "--lib".into(),
            self.lib.to_string_lossy().to_string(),
            // Write compiled .beam to ./ebin
            "--out".into(),
            self.out.join("ebin").to_string_lossy().to_string(),
        ];
        // Add the list of modules to compile
        for module in modules {
            let path = self.out.join("build").join(module).with_extension("erl");
            args.push(path.to_string_lossy().to_string());
            self.add_build_journal(path);

            let beam_path = self.out.join("ebin").join(module).with_extension("beam");
            self.add_build_journal(beam_path);
        }
        let status = self
            .io
            .exec("escript", &args, &[], None, self.silence_subprocess_stdout)?;

        if status == 0 {
            Ok(())
        } else {
            Err(Error::ShellCommand {
                program: "escript".to_string(),
                err: None,
            })
        }
    }

    fn copy_project_native_files(
        &mut self,
        build_dir: &Path,
        modules: &mut HashSet<PathBuf>,
    ) -> Result<(), Error> {
        tracing::info!("copying_native_source_files");

        // TODO: unit test
        let priv_source = self.root.join("priv");
        let priv_build = self.out.join("priv");
        if self.io.is_directory(&priv_source) && !self.io.is_directory(&priv_build) {
            tracing::debug!("linking_priv_to_build");
            self.io.symlink_dir(&priv_source, &priv_build)?;
        }

        let src = self.root.join("src");
        let test = self.root.join("test");
        let mut copied = HashSet::new();
        self.copy_native_files(&src, build_dir, &mut copied, modules)?;
        if self.io.is_directory(&test) {
            self.copy_native_files(&test, build_dir, &mut copied, modules)?;
        }

        Ok(())
    }

    fn copy_native_files(
        &mut self,
        src_path: &Path,
        out: &Path,
        copied: &mut HashSet<PathBuf>,
        to_compile_modules: &mut HashSet<PathBuf>,
    ) -> Result<()> {
        self.io.mkdir(&out)?;

        for entry in self.io.read_dir(src_path)? {
            let path = entry.expect("copy_native_files dir_entry").pathbuf;

            let extension = path
                .extension()
                .unwrap_or_default()
                .to_str()
                .unwrap_or_default();
            let relative_path = path
                .strip_prefix(src_path)
                .expect("copy_native_files strip prefix")
                .to_path_buf();

            match extension {
                "mjs" | "js" | "hrl" => (),
                "erl" => {
                    let _ = to_compile_modules.insert(relative_path.clone());
                }
                _ => continue,
            };

            self.io.copy(&path, &out.join(&relative_path))?;
            self.add_build_journal(out.join(&relative_path));

            // TODO: test
            if !copied.insert(relative_path.clone()) {
                return Err(Error::DuplicateSourceFile {
                    file: relative_path.to_string_lossy().to_string(),
                });
            }
        }
        Ok(())
    }

    fn encode_and_write_metadata(&mut self, modules: &[Module]) -> Result<()> {
        if !self.write_metadata {
            tracing::info!("Package metadata writing disabled");
            return Ok(());
        }
        tracing::info!("Writing package metadata to disc");
        for module in modules {
            let name = format!("{}.gleam_module", &module.name.replace('/', "@"));
            let path = self.out.join("build").join(name);
            ModuleEncoder::new(&module.ast.type_info).write(self.io.writer(&path)?)?;
            self.add_build_journal(path);
        }
        Ok(())
    }

    pub fn read_source_files(&mut self, mode: Mode) -> Result<()> {
        let span = tracing::info_span!("load", package = %self.config.name.as_str());
        let _enter = span.enter();
        tracing::info!("Reading source files");
        let src = self.root.join("src");
        let test = self.root.join("test");

        // Src
        for path in self.io.gleam_source_files(&src) {
            self.add_module(path, &src, Origin::Src)?;
        }

        // Test
        if mode.is_dev() && self.io.is_directory(&test) {
            for path in self.io.gleam_source_files(&test) {
                self.add_module(path, &test, Origin::Test)?;
            }
        }
        Ok(())
    }

    fn add_module(&mut self, path: PathBuf, dir: &Path, origin: Origin) -> Result<()> {
        let name = module_name(&dir, &path);
        let code = self.io.read(&path)?;
        self.sources.push(Source {
            name,
            path,
            code,
            origin,
        });
        Ok(())
    }

    fn add_build_journal(&mut self, path: PathBuf) -> Result<()> {
        if let Some(b) = self.build_journal.as_mut() {
            let _ = b.insert(path);
        }
        Ok(())
    }

    fn perform_codegen(&mut self, modules: &[Module]) -> Result<()> {
        if !self.perform_codegen {
            tracing::info!("skipping_codegen");
            return Ok(());
        }

        match self.target {
            Target::JavaScript => self.perform_javascript_codegen(modules),
            Target::Erlang => self.perform_erlang_codegen(modules),
        }
    }

    fn perform_erlang_codegen(&mut self, modules: &[Module]) -> Result<(), Error> {
        let mut written = HashSet::new();
        let build_dir = self.out.join("build");
        let include_dir = self.out.join("include");
        let io = self.io.clone();

        Erlang::new(&build_dir, &include_dir).render(io.clone(), modules)?;
        ErlangApp::new(&self.out.join("ebin")).render(io, &self.config, modules)?;

        if self.write_entrypoint {
            self.render_entrypoint_module(&build_dir, &mut written)?;
        } else {
            tracing::info!("skipping_entrypoint_generation");
        }

        if self.copy_native_files {
            self.copy_project_native_files(&build_dir, &mut written)?;
        } else {
            tracing::info!("skipping_native_file_copying");
        }

        if self.compile_beam_bytecode {
            written.extend(modules.iter().map(Module::compiled_erlang_path));
            self.compile_erlang_to_beam(&written)?;
        } else {
            tracing::info!("skipping_erlang_bytecode_compilation");
        }
        Ok(())
    }

    fn perform_javascript_codegen(&mut self, modules: &[Module]) -> Result<(), Error> {
        let mut written = HashSet::new();
        let artifact_dir = self.out.join("dist");

        JavaScript::new(&artifact_dir).render(&self.io, modules)?;

        if self.copy_native_files {
            self.copy_project_native_files(&artifact_dir, &mut written)?;
        }
        Ok(())
    }

    fn render_entrypoint_module(
        &mut self,
        out: &Path,
        modules_to_compile: &mut HashSet<PathBuf>,
    ) -> Result<(), Error> {
        let name = "gleam@@main.erl";
        let module = ErlangEntrypointModule {
            application: &self.config.name,
        }
        .render()
        .expect("Erlang entrypoint rendering");
        self.io.writer(&out.join(name))?.write(module.as_bytes())?;
        let _ = modules_to_compile.insert(name.into());
        self.add_build_journal(out.join(name));
        Ok(())
    }
}

fn type_check(
    package_name: &str,
    target: Target,
    ids: &UniqueIdGenerator,
    sequence: Vec<String>,
    mut parsed_modules: HashMap<String, Parsed>,
    module_types: &mut im::HashMap<String, type_::Module>,
    warnings: &mut Vec<Warning>,
) -> Result<Vec<Module>, Error> {
    let mut modules = Vec::with_capacity(parsed_modules.len() + 1);

    // Insert the prelude
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = module_types.insert("gleam".to_string(), type_::build_prelude(ids));

    for name in sequence {
        let Parsed {
            name,
            code,
            ast,
            path,
            origin,
            package,
            extra,
        } = parsed_modules
            .remove(&name)
            .expect("Getting parsed module for name");

        tracing::debug!(module = ?name, "Type checking");
        let mut type_warnings = Vec::new();
        let ast = type_::infer_module(
            target,
            ids,
            ast,
            origin,
            package_name,
            module_types,
            &mut type_warnings,
        )
        .map_err(|error| Error::Type {
            path: path.clone(),
            src: code.clone(),
            error,
        })?;

        // Register any warnings emitted as type warnings
        let type_warnings = type_warnings
            .into_iter()
            .map(|w| w.into_warning(path.clone(), code.clone()));
        warnings.extend(type_warnings);

        // Register the types from this module so they can be imported into
        // other modules.
        let _ = module_types.insert(name.clone(), ast.type_info.clone());

        // Register the successfully type checked module data so that it can be
        // used for code generation
        modules.push(Module {
            origin,
            extra,
            name,
            code,
            ast,
            input_path: path,
        });
    }

    Ok(modules)
}

fn convert_deps_tree_error(e: dep_tree::Error) -> Error {
    match e {
        dep_tree::Error::Cycle(modules) => Error::ImportCycle { modules },
    }
}

fn module_deps_for_graph(target: Target, module: &Parsed) -> (String, Vec<String>) {
    let name = module.name.clone();
    let deps: Vec<_> = module
        .ast
        .dependencies(target)
        .into_iter()
        .map(|(dep, _span)| dep)
        .collect();
    (name, deps)
}

fn parse_sources(
    package_name: &str,
    sources: Vec<Source>,
    already_defined_modules: &mut im::HashMap<String, PathBuf>,
) -> Result<HashMap<String, Parsed>, Error> {
    let mut parsed_modules = HashMap::with_capacity(sources.len());
    for Source {
        name,
        code,
        path,
        origin,
    } in sources
    {
        let (mut ast, extra) = crate::parse::parse_module(&code).map_err(|error| Error::Parse {
            path: path.clone(),
            src: code.clone(),
            error,
        })?;

        // Store the name
        // TODO: store the module name as a string
        ast.name = name.split("/").map(String::from).collect();

        let module = Parsed {
            package: package_name.to_string(),
            origin,
            extra,
            path,
            name,
            code,
            ast,
        };

        // Ensure there are no modules defined that already have this name
        if let Some(first) =
            already_defined_modules.insert(module.name.clone(), module.path.clone())
        {
            return Err(Error::DuplicateModule {
                module: module.name.clone(),
                first,
                second: module.path.clone(),
            });
        }

        // Register the parsed module
        let _ = parsed_modules.insert(module.name.clone(), module);
    }
    Ok(parsed_modules)
}

fn module_name(package_path: &Path, full_module_path: &Path) -> String {
    // /path/to/project/_build/default/lib/the_package/src/my/module.gleam

    // my/module.gleam
    let mut module_path = full_module_path
        .strip_prefix(package_path)
        .expect("Stripping package prefix from module path")
        .to_path_buf();

    // my/module
    let _ = module_path.set_extension("");

    // Stringify
    let name = module_path
        .to_str()
        .expect("Module name path to str")
        .to_string();

    // normalise windows paths
    name.replace("\\", "/")
}

#[derive(Debug)]
pub struct Source {
    pub path: PathBuf,
    pub name: String,
    pub code: String,
    pub origin: Origin, // TODO: is this used?
}

#[derive(Debug)]
struct Parsed {
    path: PathBuf,
    name: String,
    code: String,
    origin: Origin,
    package: String,
    ast: UntypedModule,
    extra: ModuleExtra,
}

#[derive(Template)]
#[template(path = "gleam@@main.erl", escape = "none")]
struct ErlangEntrypointModule<'a> {
    application: &'a str,
}
