#![allow(warnings)]

// TODO: Compilation of ./test
// TODO: Avoid rebuilding clean modules
// TODO: Download deps from Hex
// TODO: Support compilation of rebar3 packages
// TODO: Support flexible compiler interface for use by rebar3 + mix
// TODO: Track removed files in src and test so they can be removed from _build

mod dep_tree;
mod erlang_code_generator;
mod package_analyser;
mod project_analyser;
pub mod project_root;

use crate::{
    ast::TypedModule,
    build::{
        erlang_code_generator::ErlangCodeGenerator, project_analyser::ProjectAnalyser,
        project_root::ProjectRoot,
    },
    config::{self, PackageConfig},
    erl,
    error::{Error, FileIOAction, FileKind, GleamExpect},
    file::{self, OutputFile},
    grammar, parser, typ,
};
use itertools::Itertools;
use std::collections::HashMap;
use std::ffi::OsString;
use std::fs::DirEntry;
use std::path::PathBuf;
use std::process;

pub fn main(root_config: PackageConfig, root: PathBuf) -> Result<(), Error> {
    let root = ProjectRoot::new(root);

    // Copy any native Erlang source code from src to _build
    copy_erlang_code_to_build(&root, &root_config)?;

    // Collect all package configs
    let configs = root.package_configs()?;

    // Read and type check all packages in project
    let packages = ProjectAnalyser::new(&root, configs).analyse()?;

    // Generate Erlang source code
    let compiled_erlang = ErlangCodeGenerator::new(&root, &packages).render();

    // Write compiled Erlang disc
    file::write_outputs(compiled_erlang.as_slice())?;

    // Compile Erlang source into VM bytecode
    compile_erlang_to_beam(&root, &packages)?;

    Ok(())
}

#[derive(Debug)]
pub struct Package {
    config: PackageConfig,
    modules: Vec<Module>,
}

#[derive(Debug)]
pub struct Module {
    name: String,
    code: String,
    path: PathBuf,
    ast: TypedModule,
}

fn compile_erlang_to_beam(
    root: &ProjectRoot,
    packages: &HashMap<String, Package>,
) -> Result<(), Error> {
    let escript_path = root.build_path().join("compile_escript.erl");
    let escript_source = std::include_str!("build/compile_escript.erl").to_string();

    file::write_output(&OutputFile {
        path: escript_path.clone(),
        text: escript_source,
    })?;

    // Run escript to compile Erlang to beam files
    let status = process::Command::new("escript")
        .arg(escript_path)
        .arg(root.build_path())
        .status()
        .unwrap(); // TODO

    // TODO: check status

    Ok(())
}

// TODO: Test the copying of native erlang module, possibly with a integration test
fn copy_erlang_code_to_build(root: &ProjectRoot, config: &PackageConfig) -> Result<(), Error> {
    let erl = OsString::from("erl");
    for src_file in file::read_dir(root.src_path())?.filter_map(Result::ok) {
        let path = src_file.path();
        if path.extension() == Some(erl.as_os_str()) {
            let dest = root
                .default_build_lib_package_src_path(&config.name)
                .join(src_file.file_name());
            file::copy(&path, dest)?;
        }
    }
    Ok(())
}
