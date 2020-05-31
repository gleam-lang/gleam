use crate::{
    ast::UntypedModule,
    build::{project_root::ProjectRoot, Module, Package},
    config::PackageConfig,
    error::{self, Error, GleamExpect},
    file, grammar, parser, typ,
};
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug)]
pub struct PackageAnalyser<'a> {
    pub root: &'a ProjectRoot,
    pub config: &'a PackageConfig,
    pub sources: Vec<Source>,
}

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
        _existing_modules: &HashMap<&str, typ::Module>,
    ) -> Result<Package<'a>, Error> {
        let mut modules = HashMap::with_capacity(self.sources.len());
        for Source { name, code, path } in self.sources.into_iter() {
            let ast = parse_source(code.as_str(), name.as_str(), &path)?;

            // TODO: type check
            // TODO: ensure this is not a duplicate module

            let module = Module { name, code, ast };

            modules.insert(module.name.clone(), module);
        }

        Ok(Package {
            name: self.config.name.as_str(),
            modules,
        })
    }

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
