use crate::{
    ast::{SrcSpan, TypedModule, UntypedModule},
    build::{dep_tree, project_root::ProjectRoot, Module, Origin, Package},
    config::PackageConfig,
    error,
    fs::FileWriter,
    grammar, parser, typ, CodeGenerator, Error, GleamExpect, Warning,
};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct PackageCompiler {
    pub config: PackageConfig,
    pub sources: Vec<Source>,
    pub print_progress: bool,
    pub code_generators: Vec<Box<dyn CodeGenerator>>,
    pub writer: Box<dyn FileWriter>,
}

// TODO: ensure this is not a duplicate module
// TODO: tests
// Including cases for:
// - modules that don't import anything
impl PackageCompiler {
    pub fn new(config: PackageConfig, writer: Box<dyn FileWriter>) -> Self {
        Self {
            config,
            writer,
            sources: vec![],
            print_progress: true,
            code_generators: vec![],
        }
    }

    pub fn with_code_generator(mut self, code_generator: Box<dyn CodeGenerator>) -> Self {
        self.code_generators.push(code_generator);
        self
    }

    pub fn compile(
        self,
        existing_modules: &mut HashMap<String, (Origin, typ::Module)>,
        already_defined_modules: &mut HashMap<String, PathBuf>,
    ) -> Result<Package, Error> {
        if self.print_progress {
            crate::cli::print_compiling(self.config.name.as_str());
        }
        let Self {
            sources,
            code_generators,
            config,
            ..
        } = self;

        let span = tracing::info_span!("compile", package = config.name.as_str());
        let _enter = span.enter();

        tracing::info!("Parsing source code");
        let parsed_modules = parse_sources(sources, already_defined_modules)?;

        // Determine order in which modules are to be processed
        let sequence =
            dep_tree::toposort_deps(parsed_modules.values().map(module_deps_for_graph).collect())
                .map_err(convert_deps_tree_error)?;

        tracing::info!("Type checking modules");
        let modules = type_check(sequence, parsed_modules, existing_modules)?;

        tracing::info!("Running code generators");
        for code_generator in code_generators {
            code_generator.render(self.writer.as_ref(), &config, modules.as_slice())?;
        }

        Ok(Package { config, modules })
    }

    pub fn read_package_source_files(
        &mut self,
        package_path: &Path,
        origin: Origin,
    ) -> Result<(), Error> {
        let span =
            tracing::info_span!("load", package = self.config.name.as_str(), origin = ?origin);
        let _enter = span.enter();

        tracing::info!("Reading source code");
        for path in crate::fs::gleam_files(package_path) {
            let name = module_name(package_path, &path);
            let code = crate::fs::read(&path)?;
            self.sources.push(Source {
                name,
                path,
                code,
                origin,
            });
        }
        Ok(())
    }
}

fn type_check(
    sequence: Vec<String>,
    mut parsed_modules: HashMap<String, Parsed>,
    module_types: &mut HashMap<String, (Origin, typ::Module)>,
) -> Result<Vec<Module>, Error> {
    let mut warnings = vec![];
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
        let ast =
            typ::infer_module(&mut uid, ast, module_types, &mut warnings).map_err(|error| {
                Error::Type {
                    path: path.clone(),
                    src: code.clone(),
                    error,
                }
            })?;

        module_types.insert(name.clone(), (origin, ast.type_info.clone()));

        modules.push(Module {
            origin,
            name,
            code,
            ast,
            path,
        });
    }

    // TODO: do something with warnings

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
        let ast = parse_source(code.as_str(), name.as_str(), &path)?;
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

fn parse_source(src: &str, name: &str, path: &PathBuf) -> Result<UntypedModule, Error> {
    // Strip comments, etc
    let (cleaned, comments) = parser::strip_extra(src);

    // Parse source into AST
    let mut module = grammar::ModuleParser::new()
        .parse(&cleaned)
        .map_err(|e| Error::Parse {
            path: path.clone(),
            src: src.to_string(),
            error: e.map_token(|crate::grammar::Token(a, b)| (a, b.to_string())),
        })?;

    // Attach documentation
    parser::attach_doc_comments(&mut module, &comments.doc_comments);
    module.documentation = comments
        .module_comments
        .iter()
        .map(|s| s.to_string())
        .collect();

    // Store the name
    module.name = name.split("/").map(String::from).collect(); // TODO: store the module name as a string

    Ok(module)
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
