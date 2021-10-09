use crate::{
    ast::{SrcSpan, TypedModule, UntypedModule},
    build::{dep_tree, project_root::ProjectRoot, Module, Origin, Package, Target},
    codegen::{Erlang, JavaScript},
    config::PackageConfig,
    error,
    io::{FileSystemIO, FileSystemReader, FileSystemWriter},
    metadata::ModuleEncoder,
    type_, Error, Result, Warning,
};
use std::path::{Path, PathBuf};
use std::{collections::HashMap, fmt::write};

#[derive(Debug)]
pub struct Options {
    pub target: Target,
    pub name: String,
    pub src_path: PathBuf,
    pub test_path: Option<PathBuf>,
    pub out_path: PathBuf,
}

impl Options {
    pub fn into_compiler<IO>(self, io: IO) -> Result<PackageCompiler<IO>>
    where
        IO: FileSystemIO + Clone,
    {
        let mut compiler = PackageCompiler {
            options: self,
            sources: vec![],
            write_metadata: false,
            io,
        };
        compiler.read_source_files()?;
        Ok(compiler)
    }
}

#[derive(Debug)]
pub struct PackageCompiler<IO> {
    pub options: Options,
    pub sources: Vec<Source>,
    pub io: IO,
    pub write_metadata: bool,
}

// TODO: ensure this is not a duplicate module
// TODO: tests
// Including cases for:
// - modules that don't import anything
impl<IO> PackageCompiler<IO>
where
    IO: FileSystemIO + Clone,
{
    pub fn new(options: Options, io: IO) -> Self {
        Self {
            io,
            options,
            sources: vec![],
            write_metadata: false,
        }
    }

    pub fn compile(
        mut self,
        warnings: &mut Vec<Warning>,
        existing_modules: &mut HashMap<String, type_::Module>,
        already_defined_modules: &mut HashMap<String, PathBuf>,
    ) -> Result<Package, Error> {
        let span = tracing::info_span!("compile", package = self.options.name.as_str());
        let _enter = span.enter();

        tracing::info!("Parsing source code");
        let parsed_modules = parse_sources(
            &self.options.name,
            std::mem::take(&mut self.sources),
            already_defined_modules,
        )?;

        // Determine order in which modules are to be processed
        let sequence = dep_tree::toposort_deps(
            parsed_modules
                .values()
                .map(|m| module_deps_for_graph(self.options.target, m))
                .collect(),
        )
        .map_err(convert_deps_tree_error)?;

        tracing::info!("Type checking modules");
        let modules = type_check(
            &self.options.name,
            self.options.target,
            sequence,
            parsed_modules,
            existing_modules,
            warnings,
        )?;

        tracing::info!("Performing code generation");
        self.perform_codegen(&modules)?;

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
            tracing::info!(name = %name, "Writing module metadata");
            let path = self.options.out_path.join(name);
            ModuleEncoder::new(&module.ast.type_info).write(self.io.writer(&path)?)?;
        }
        Ok(())
    }

    pub fn read_source_files(&mut self) -> Result<()> {
        let span = tracing::info_span!("load", package = self.options.name.as_str());
        let _enter = span.enter();
        tracing::info!("Reading source files");

        // Src
        for path in self.io.gleam_files(&self.options.src_path) {
            let name = module_name(&self.options.src_path, &path);
            let code = self.io.read(&path)?;
            self.sources.push(Source {
                name,
                path,
                code,
                origin: Origin::Src,
            });
        }

        // Test
        if let Some(test_path) = &self.options.test_path {
            for path in self.io.gleam_files(test_path) {
                let name = module_name(test_path, &path);
                let code = self.io.read(&path)?;
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
        match self.options.target {
            Target::JavaScript => JavaScript::new(&self.options.out_path).render(&self.io, modules),
            Target::Erlang => Erlang::new(&self.options.out_path).render(self.io.clone(), modules),
        }
    }

    /// Set whether to write metadata files
    pub fn write_metadata(mut self, write_metadata: bool) -> Self {
        self.write_metadata = write_metadata;
        self
    }
}

fn type_check(
    package_name: &str,
    target: Target,
    sequence: Vec<String>,
    mut parsed_modules: HashMap<String, Parsed>,
    module_types: &mut HashMap<String, type_::Module>,
    warnings: &mut Vec<Warning>,
) -> Result<Vec<Module>, Error> {
    let mut modules = Vec::with_capacity(parsed_modules.len() + 1);
    let mut uid = 0;

    // Insert the prelude
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = module_types.insert("gleam".to_string(), type_::build_prelude(&mut uid));

    for name in sequence {
        let Parsed {
            name,
            code,
            ast,
            path,
            origin,
            package,
        } = parsed_modules
            .remove(&name)
            .expect("Getting parsed module for name");

        tracing::trace!(module = ?name, "Type checking");
        let mut type_warnings = Vec::new();
        let ast = type_::infer_module(
            target,
            &mut uid,
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
    already_defined_modules: &mut HashMap<String, PathBuf>,
) -> Result<HashMap<String, Parsed>, Error> {
    let mut parsed_modules = HashMap::with_capacity(sources.len());
    for Source {
        name,
        code,
        path,
        origin,
    } in sources
    {
        let (mut ast, _) = crate::parse::parse_module(&code).map_err(|error| Error::Parse {
            path: path.clone(),
            src: code.clone(),
            error,
        })?;

        // Store the name
        ast.name = name.split("/").map(String::from).collect(); // TODO: store the module name as a string

        let module = Parsed {
            package: package_name.to_string(),
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
}
