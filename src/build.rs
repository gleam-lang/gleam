// ## Get all gleam.toml
//
// Read the top level gleam.toml
// Read the gleam.toml for each of the dependencies
// Loop for each of their deps until we have them all
// TODO: ensure the gleam.toml name matches the project name
//
//
// ## Sort packages into processing order
//
// Build deps graph for packages (petgraph)
// Toposort graph to get processing order
//
//
// ## Process each package
//
// For each package:
//
// Read and parse all the module source files for the package
//
//    {
//      ast: ast::UntypedModule
//      code: String,
//      package: String,
//    }
//
// Build deps graph for modules
// Toposort graph to get processing order
//
// For each module:
// Run the typer
//   When importing a module ensure that it is a module that belongs
//   to a package in the deps of the current package.
//
//    {
//      ast: ast::TypedModule
//      manifest: ModuleTypeManifest
//      code: String,
//      package: String,
//    }
//

#![allow(warnings)]

mod dep_tree;

use crate::{
    config::{self, PackageConfig},
    error::{Error, FileIOAction, FileKind},
    file,
};
use itertools::Itertools;
use std::collections::HashMap;
use std::fs::DirEntry;
use std::path::PathBuf;

// Directory names
const DIR_NAME_BUILD: &str = "_build";
const DIR_NAME_PROFILE_DEFAULT: &str = "default";
const DIR_NAME_LIB: &str = "lib";
const DIR_NAME_PACKAGE_SRC: &str = "src";
const DIR_NAME_PACKAGE_EBIN: &str = "ebin";

pub fn main(package_config: PackageConfig, root: PathBuf) -> Result<(), Error> {
    let root = ProjectRoot::new(root);

    // Collect all package configs
    let packages = root.package_configs()?;

    // Determine package processing order
    let deps: Vec<_> = packages
        .values()
        .map(|config| {
            let name = config.name.as_str();
            let deps: Vec<_> = config
                .dependencies
                .iter()
                .map(|(dep, _)| dep.as_str())
                .collect();
            (name, deps)
        })
        .collect();

    let sequence = dep_tree::toposort_deps(deps.as_slice());

    dbg!(&sequence);

    todo!()
}

struct ProjectRoot {
    path: PathBuf,
}

impl ProjectRoot {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }

    pub fn package_configs(&self) -> Result<HashMap<String, PackageConfig>, Error> {
        file::read_dir(self.default_build_lib_path())?
            .filter_map(Result::ok)
            .map(|dir_entry| {
                let config = config::read_project_config(dir_entry.path())?;
                Ok((config.name.clone(), config))
            })
            .collect::<Result<_, _>>()
    }

    pub fn default_build_lib_path(&self) -> PathBuf {
        self.path
            .join(DIR_NAME_BUILD)
            .join(DIR_NAME_PROFILE_DEFAULT)
            .join(DIR_NAME_LIB)
    }
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
