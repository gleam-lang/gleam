#![allow(warnings)]

mod dep_tree;
mod project_root;

use crate::{
    ast::UntypedModule,
    config::{self, PackageConfig},
    error::{Error, FileIOAction, FileKind, GleamExpect},
    file, grammar, parser, typ,
};
use itertools::Itertools;
use project_root::ProjectRoot;
use std::collections::HashMap;
use std::fs::DirEntry;
use std::path::PathBuf;

pub fn main(package_config: PackageConfig, root: PathBuf) -> Result<(), Error> {
    let root = ProjectRoot::new(root);

    // Collect all package configs
    let configs = root.package_configs()?;

    // Determine package processing order
    let deps: Vec<_> = configs.values().map(package_deps_for_graph).collect();
    let sequence = dep_tree::toposort_deps(deps.as_slice()).map_err(convert_deps_tree_error)?;

    // Read and type check packages
    let packages = analyse_packages(&root, &configs, sequence)?;

    // TODO: generate Erlang, etc
    // TODO: compile Erlang into .beam

    dbg!(&packages);

    todo!()
}

fn analyse_packages<'a>(
    root: &ProjectRoot,
    configs: &'a HashMap<String, PackageConfig>,
    sequence: Vec<&'a str>,
) -> Result<HashMap<&'a str, Package<'a>>, Error> {
    let mut packages = HashMap::with_capacity(configs.len());
    let mut public_modules = HashMap::with_capacity(configs.len() * 5);

    for name in sequence {
        let config = configs.get(name).gleam_expect("Missing package config");
        let analysed = analyse_package(root, config, &public_modules)?;
        packages.insert(name, analysed);
        // TODO: insert public modules for package into public_modules
    }

    Ok(packages)
}

fn analyse_package<'a>(
    root: &ProjectRoot,
    config: &'a PackageConfig,
    other_package_modules: &HashMap<&str, typ::Module>,
) -> Result<Package<'a>, Error> {
    let package_path = root.default_build_lib_package_path(&config.name);
    let mut modules = HashMap::with_capacity(20); // A guess at how many modules will be in a package

    for path in file::gleam_files(&package_path) {
        let name = module_name(root, &package_path, &path);
        let code = file::read(&path)?;
        let ast = parse_source(code.as_str(), name.as_str(), &path)?;

        // TODO: parse
        // TODO: type check
        // TODO: ensure this is not a duplicate module

        let module = Module { name, code, ast };

        modules.insert(module.name.clone(), module);
    }

    Ok(Package {
        name: config.name.as_str(),
        modules,
    })
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

fn module_name(root: &ProjectRoot, package_path: &PathBuf, full_module_path: &PathBuf) -> String {
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

#[derive(Debug, PartialEq)]
pub struct Input {
    pub name: Vec<String>,
    pub path: PathBuf,
    pub src: String,
}

#[derive(Debug)]
pub struct Package<'a> {
    name: &'a str,
    modules: HashMap<String, Module>,
}

#[derive(Debug)]
pub struct Module {
    name: String,
    code: String,
    ast: UntypedModule, // TODO: typed module
}

fn convert_deps_tree_error(e: dep_tree::Error) -> Error {
    todo!()
}

fn package_deps_for_graph(config: &PackageConfig) -> (&str, Vec<&str>) {
    let name = config.name.as_str();
    let deps: Vec<_> = config
        .dependencies
        .iter()
        .map(|(dep, _)| dep.as_str())
        .collect();
    (name, deps)
}

// // Compile Erlang to .beam files
// if config.tool == config::BuildTool::Gleam {
//     compile_erlang(output_files.as_slice());
// }
// pub fn compile_erlang(output_files: &[project::OutputFile]) {

// let projects = output_files.iter().group_by(|out| {
//     out.path
//         .parent()
//         .gleam_expect("compile_erlang path parent 1")
//         .parent()
//         .gleam_expect("compile_erlang path parent 2")
// });

// for (path, files) in projects.into_iter() {
//     let ebin_dir = path.join("ebin");
//     file::mkdir(&ebin_dir).gleam_expect("command_build mkdir ebin");
//     let ebin_dir_string = ebin_dir
//         .to_str()
//         .gleam_expect("command_build ebin_dir to_str")
//         .to_string();
//     let mut command = process::Command::new("erlc");
//     let erl_files = files
//         .filter(|out| out.path.extension().map(|s| s.to_str()) == Some(Some("erl")))
//         .map(|out| {
//             out.path
//                 .to_str()
//                 .gleam_expect("command_build out path to_str")
//                 .to_string()
//         });
//     command.arg("-o");
//     command.arg(ebin_dir_string);
//     command.args(erl_files);

//     println!("Compiling beam: {}", path.to_str().unwrap_or_default());
//     command.status().gleam_expect("Command erlc");
// }
// }
