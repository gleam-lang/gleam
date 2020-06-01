#![allow(warnings)]

mod dep_tree;
mod package_analyser;
mod project_root;

use crate::{
    ast::TypedModule,
    config::{self, PackageConfig},
    error::{Error, FileIOAction, FileKind, GleamExpect},
    file, grammar, parser, typ,
};
use itertools::Itertools;
use package_analyser::PackageAnalyser;
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
    root: &'a ProjectRoot,
    configs: &'a HashMap<String, PackageConfig>,
    sequence: Vec<&'a str>,
) -> Result<HashMap<&'a str, Package<'a>>, Error> {
    let mut packages = HashMap::with_capacity(configs.len());
    let mut module_type_manifests = HashMap::with_capacity(configs.len() * 5);

    for name in sequence {
        let config = configs.get(name).gleam_expect("Missing package config");
        let mut analyser = PackageAnalyser::new(root, config);
        analyser.read_package_source_files()?;
        let analysed = analyser.analyse(&mut module_type_manifests)?;
        packages.insert(name, analysed);
        // TODO: insert public modules for package into public_modules
    }

    Ok(packages)
}

#[derive(Debug)]
pub struct Package<'a> {
    name: &'a str,
    modules: Vec<Module>,
}

#[derive(Debug)]
pub struct Module {
    name: String,
    code: String,
    path: PathBuf,
    ast: TypedModule,
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
