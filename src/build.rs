#![allow(warnings)]

// TODO: Avoid rebuilding clean modules
// TODO: Download deps from Hex
// TODO: Support compilation of rebar3 packages
// TODO: Support flexible compiler interface for use by rebar3 + mix

mod dep_tree;
mod erlang_code_generator;
mod package_analyser;
mod project_analyser;
mod project_root;

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
use std::fs::DirEntry;
use std::path::PathBuf;
use std::process;

pub fn main(package_config: PackageConfig, root: PathBuf) -> Result<(), Error> {
    let root = ProjectRoot::new(root);

    // TODO: copy any Erlang source code from src to _build

    // Collect all package configs
    let configs = root.package_configs()?;

    // Read and type check all packages in project
    let packages = ProjectAnalyser::new(&root, &configs).analyse()?;

    // Generate Erlang source code
    let compiled_erlang = ErlangCodeGenerator::new(&root, &packages).render();

    // Write compiled Erlang disc
    file::write_outputs(compiled_erlang.as_slice())?;

    // Compile Erlang source into VM bytecode
    compile_erlang_to_beam(&root, &packages)?;

    Ok(())
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

pub fn compile_erlang_to_beam(
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

    dbg!(&status);

    // println!("Compiling beam: {}", path.to_str().unwrap_or_default());
    // command.status().gleam_expect("Command erlc");
    // }

    Ok(())
}
