use crate::{
    ast::{SrcSpan, TypedModule, UntypedModule},
    build::{dep_tree, project_root::ProjectRoot, Module, Package},
    config::PackageConfig,
    error::{self, Error, GleamExpect},
    file, grammar, parser, typ,
    warning::Warning,
};
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug)]
pub struct PackageAnalyser<'a> {
    pub root: &'a ProjectRoot,
    pub config: &'a PackageConfig,
    pub sources: Vec<Source>,
}

// TODO: ensure this is not a duplicate module
// TODO: tests
// Including cases for:
// - modules that don't import anything
// - modules with names that collide with modules in an already compiled package
impl<'a> PackageAnalyser<'a> {
    pub fn new(root: &'a ProjectRoot, config: &'a PackageConfig) -> Self {
        Self {
            root,
            config,
            sources: vec![],
        }
    }

    pub fn analyse(
        self,
        existing_modules: &mut HashMap<String, typ::Module>,
    ) -> Result<Package<'a>, Error> {
        // Parse source code into abstract syntax trees
        let parsed_modules = parse_sources(self.sources)?;

        // Determine order in which modules are to be processed
        let deps: Vec<_> = parsed_modules.values().map(module_deps_for_graph).collect();
        let str_deps = toposort_format_deps(deps.as_slice());
        let sequence =
            dep_tree::toposort_deps(str_deps.as_slice()).map_err(convert_deps_tree_error)?;

        // Type check modules
        let modules = type_check(sequence, parsed_modules, existing_modules)?;

        Ok(Package {
            name: self.config.name.as_str(),
            modules,
        })
    }

    // TODO: if we inject in this functionality with some kind of SourceProvider
    // trait then we can test the compilation of multiple packages easily.
    pub fn read_package_source_files(&mut self) -> Result<(), Error> {
        let package_path = self.root.default_build_lib_package_path(&self.config.name);
        for path in file::gleam_files(&package_path) {
            let name = module_name(&package_path, &path);
            let code = file::read(&path)?;
            self.sources.push(Source { name, path, code });
        }
        Ok(())
    }
}

fn type_check(
    sequence: Vec<&str>,
    mut parsed_modules: HashMap<String, Parsed>,
    module_types: &mut HashMap<String, typ::Module>,
) -> Result<Vec<Module>, Error> {
    let mut warnings = vec![];
    let mut modules = Vec::with_capacity(parsed_modules.len());

    for name in sequence {
        let Parsed {
            name,
            code,
            ast,
            path,
        } = parsed_modules
            .remove(name)
            .gleam_expect("Getting parsed module for name");

        let ast =
            typ::infer_module(ast, module_types, &mut warnings).map_err(|error| Error::Type {
                path: path.clone(),
                src: code.clone(),
                error,
            })?;

        module_types.insert(name.clone(), ast.type_info.clone());

        modules.push(Module {
            name,
            code,
            ast,
            path,
        });
    }

    // TODO: do something with warnings

    Ok(modules)
}

fn toposort_format_deps<'a>(
    deps: &'a [(String, Vec<(String, SrcSpan)>)],
) -> Vec<(&'a str, Vec<&'a str>)> {
    deps.iter()
        .map(|(name, deps)| {
            let deps = deps.iter().map(|(dep, _)| dep.as_str()).collect();
            (name.as_str(), deps)
        })
        .collect()
}

fn convert_deps_tree_error(e: dep_tree::Error) -> Error {
    todo!()
}

fn module_deps_for_graph(module: &Parsed) -> (String, Vec<(String, SrcSpan)>) {
    let name = module.name.clone();
    let deps: Vec<_> = module.ast.dependencies();
    (name, deps)
}

fn parse_sources(sources: Vec<Source>) -> Result<HashMap<String, Parsed>, Error> {
    let mut parsed_modules = HashMap::with_capacity(sources.len());
    for Source { name, code, path } in sources.into_iter() {
        let ast = parse_source(code.as_str(), name.as_str(), &path)?;
        let module = Parsed {
            path,
            name,
            code,
            ast,
        };
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

fn module_name(package_path: &PathBuf, full_module_path: &PathBuf) -> String {
    // /path/to/project/_build/default/lib/the_package/src/my/module.gleam

    // src/my/module.gleam
    let project_path = full_module_path
        .strip_prefix(package_path)
        .gleam_expect("Stripping package prefix from module path");

    // my/module.gleam
    let mut module_path = project_path
        .strip_prefix("src")
        .or_else(|_| project_path.strip_prefix("test"))
        .gleam_expect("Stripping src/test prefix")
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
    path: PathBuf,
    name: String,
    code: String,
}

#[derive(Debug)]
struct Parsed {
    path: PathBuf,
    name: String,
    code: String,
    ast: UntypedModule,
}
