#![allow(warnings)]

mod dep_tree;
mod package_analyser;
mod project_analyser;
mod project_root;

use crate::{
    ast::TypedModule,
    build::{project_analyser::ProjectAnalyser, project_root::ProjectRoot},
    config::{self, PackageConfig},
    error::{Error, FileIOAction, FileKind, GleamExpect},
    file, grammar, parser, typ,
};
use itertools::Itertools;
use std::collections::HashMap;
use std::fs::DirEntry;
use std::path::PathBuf;

pub fn main(package_config: PackageConfig, root: PathBuf) -> Result<(), Error> {
    let root = ProjectRoot::new(root);

    // Collect all package configs
    let configs = root.package_configs()?;

    // Read and type check all packages in project
    let packages = ProjectAnalyser::new(&root, &configs).analyse()?;

    // TODO: generate Erlang, etc
    // TODO: compile Erlang into .beam

    dbg!(&packages);

    todo!()
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
