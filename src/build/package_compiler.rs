use crate::{
    ast::{SrcSpan, TypedModule, UntypedModule},
    build::{dep_tree, project_root::ProjectRoot, Module, Origin, Package},
    codegen::Erlang,
    config::PackageConfig,
    error,
    fs::FileSystemWriter,
    metadata::ModuleEncoder,
    type_, Error, GleamExpect, Result, Warning,
};
use std::path::{Path, PathBuf};
use std::{collections::HashMap, fmt::write};

#[derive(Debug)]
pub struct Options {
    pub name: String,
    pub src_path: PathBuf,
    pub test_path: Option<PathBuf>,
    pub out_path: PathBuf,
}

impl Options {
    pub fn into_compiler<Writer: FileSystemWriter>(
        self,
        writer: Writer,
    ) -> Result<PackageCompiler<Writer>> {
        let mut compiler = PackageCompiler {
            options: self,
            sources: vec![],
            writer,
            write_metadata: false,
        };
        compiler.read_source_files()?;
        Ok(compiler)
    }
}

#[derive(Debug)]
pub struct PackageCompiler<Writer: FileSystemWriter> {
    pub options: Options,
    pub sources: Vec<Source>,
    pub writer: Writer,
    pub write_metadata: bool,
}

// TODO: ensure this is not a duplicate module
// TODO: tests
// Including cases for:
// - modules that don't import anything
impl<Writer: FileSystemWriter> PackageCompiler<Writer> {
    pub fn new(options: Options, writer: Writer) -> Self {
        Self {
            options,
            writer,
            sources: vec![],
            write_metadata: false,
        }
    }

    pub fn compile(
        mut self,
        warnings: &mut Vec<Warning>,
        existing_modules: &mut HashMap<String, (Origin, type_::Module)>,
        already_defined_modules: &mut HashMap<String, PathBuf>,
    ) -> Result<Package, Error> {
        let span = tracing::info_span!("compile", package = self.options.name.as_str());
        let _enter = span.enter();

        tracing::info!("Parsing source code");
        let parsed_modules =
            parse_sources(std::mem::take(&mut self.sources), already_defined_modules)?;

        // Determine order in which modules are to be processed
        let sequence =
            dep_tree::toposort_deps(parsed_modules.values().map(module_deps_for_graph).collect())
                .map_err(convert_deps_tree_error)?;

        tracing::info!("Type checking modules");
        let modules = type_check(sequence, parsed_modules, existing_modules, warnings)?;

        tracing::info!("Performing code generation");
        self.perform_codegen(modules.as_slice())?;

        tracing::info!("Writing package metadata to disc");
        self.encode_and_write_metadata(&modules)?;

        Ok(Package {
            name: self.options.name,
            modules,
        })
    }

    fn encode_and_write_metadata(&mut self, modules: &[Module]) -> Result<()> {
        if !self.write_metadata {
            return Ok(());
        }
        for module in modules {
            let name = format!("{}.gleam_module", &module.name.replace('/', "@"));
            tracing::trace!(name = %name, "Writing module metadata");
            let path = self.options.out_path.join(name);
            ModuleEncoder::new(&module.ast.type_info).write(self.writer.open(&path)?)?;
        }
        Ok(())
    }

    pub fn read_source_files(&mut self) -> Result<()> {
        let span = tracing::info_span!("load", package = self.options.name.as_str());
        let _enter = span.enter();
        tracing::info!("Reading source files");

        // Src
        for path in crate::fs::gleam_files(&self.options.src_path) {
            let name = module_name(&self.options.src_path, &path);
            let code = crate::fs::read(&path)?;
            self.sources.push(Source {
                name,
                path,
                code,
                origin: Origin::Src,
            });
        }

        // Test
        if let Some(test_path) = &self.options.test_path {
            for path in crate::fs::gleam_files(test_path) {
                let name = module_name(test_path, &path);
                let code = crate::fs::read(&path)?;
                self.sources.push(Source {
                    name,
                    path,
                    code,
                    origin: Origin::Test,
                });
            }
        }
        Ok(())
    }

    fn perform_codegen(&self, modules: &[Module]) -> Result<()> {
        Erlang::new(self.options.out_path.as_path()).render(&self.writer, modules)
    }

    /// Set whether to write metadata files
    pub fn write_metadata(mut self, write_metadata: bool) -> Self {
        self.write_metadata = write_metadata;
        self
    }
}

fn type_check(
    sequence: Vec<String>,
    mut parsed_modules: HashMap<String, Parsed>,
    module_types: &mut HashMap<String, (Origin, type_::Module)>,
    warnings: &mut Vec<Warning>,
) -> Result<Vec<Module>, Error> {
    let mut modules = Vec::with_capacity(parsed_modules.len());
    let mut uid = 0;

    for name in sequence {
        let Parsed {
            name,
            code,
            ast,
            path,
            origin,
        } = parsed_modules
            .remove(&name)
            .gleam_expect("Getting parsed module for name");

        tracing::trace!(module = ?name, "Type checking");
        let mut type_warnings = Vec::new();
        let ast = type_::infer_module(&mut uid, ast, module_types, &mut type_warnings).map_err(
            |error| Error::Type {
                path: path.clone(),
                src: code.clone(),
                error,
            },
        )?;

        // Register any warnings emitted as type warnings
        let type_warnings = type_warnings
            .into_iter()
            .map(|w| w.to_warning(path.clone(), code.clone()));
        warnings.extend(type_warnings);

        // Register the types from this module so they can be imported into
        // other modules.
        module_types.insert(name.clone(), (origin, ast.type_info.clone()));

        // Register the successfully type checked module data so that it can be
        // used for code generation
        modules.push(Module {
            origin,
            name,
            code,
            ast,
            path,
        });
    }

    Ok(modules)
}

fn convert_deps_tree_error(e: dep_tree::Error) -> Error {
    match e {
        dep_tree::Error::Cycle(modules) => Error::ImportCycle { modules },
    }
}

fn module_deps_for_graph(module: &Parsed) -> (String, Vec<String>) {
    let name = module.name.clone();
    let deps: Vec<_> = module
        .ast
        .dependencies()
        .into_iter()
        .map(|(dep, _span)| dep)
        .collect();
    (name, deps)
}

fn parse_sources(
    sources: Vec<Source>,
    already_defined_modules: &mut HashMap<String, PathBuf>,
) -> Result<HashMap<String, Parsed>, Error> {
    let mut parsed_modules = HashMap::with_capacity(sources.len());
    for source in sources.into_iter() {
        let Source {
            name,
            code,
            path,
            origin,
        } = source;
        let (mut ast, _) =
            crate::parse::parse_module(code.as_str()).map_err(|error| Error::Parse {
                path: path.clone(),
                src: code.clone(),
                error,
            })?;

        // Store the name
        ast.name = name.as_str().split("/").map(String::from).collect(); // TODO: store the module name as a string

        let module = Parsed {
            origin,
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
        parsed_modules.insert(module.name.clone(), module);
    }
    Ok(parsed_modules)
}

fn module_name(package_path: &Path, full_module_path: &Path) -> String {
    // /path/to/project/_build/default/lib/the_package/src/my/module.gleam

    // my/module.gleam
    let mut module_path = full_module_path
        .strip_prefix(package_path)
        .gleam_expect("Stripping package prefix from module path")
        .to_path_buf();

    // my/module
    module_path.set_extension("");

    // Stringify
    let name = module_path
        .to_str()
        .gleam_expect("Module name path to str")
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
    ast: UntypedModule,
}
