#![allow(warnings)]

// TODO: Avoid rebuilding clean modules
// TODO: Download deps from Hex
// TODO: Support compilation of rebar3 packages
// TODO: Track removed files in src and test so they can be removed from _build
// TODO: Test profile and default profile
// TODO: Only compile test code in test profile
// TODO: Full .app generation
// TODO: Validate config.otp_start_module does not contain '
// TODO: Validate config.otp_start_module has a start function
// - custom output paths
// - no .app generation
// - no Erlang generation

pub mod compile_package;
mod dep_tree;
pub mod package_compiler;
mod project_compiler;
pub mod project_root;

#[cfg(test)]
mod package_compilation_tests;

pub use self::package_compiler::PackageCompiler;

use crate::{
    ast::TypedModule,
    build::{project_compiler::ProjectCompiler, project_root::ProjectRoot},
    config::{self, PackageConfig},
    erl,
    error::{Error, FileIOAction, FileKind, GleamExpect},
    fs::OutputFile,
    type_,
};
use itertools::Itertools;
use std::{collections::HashMap, ffi::OsString, fs::DirEntry, path::PathBuf, process};

pub fn main(root_config: PackageConfig, path: PathBuf) -> Result<HashMap<String, Package>, Error> {
    let root = ProjectRoot::new(path);

    tracing::info!("Copying root package to _build");
    copy_root_package_to_build(&root, &root_config)?;

    tracing::info!("Reading package configs from _build");
    let configs = root.package_configs(&root_config.name)?;

    tracing::info!("Compiling packages");
    let packages = ProjectCompiler::new(&root, root_config, configs).compile()?;

    tracing::info!("Compiling Erlang source code to BEAM bytecode");
    compile_erlang_to_beam(&root, &packages)?;

    Ok(packages)
}

#[derive(Debug)]
pub struct Package {
    pub name: String,
    pub modules: Vec<Module>,
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub code: String,
    pub path: PathBuf,
    pub origin: Origin,
    pub ast: TypedModule,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Origin {
    Src,
    Test,
}

fn compile_erlang_to_beam(
    root: &ProjectRoot,
    packages: &HashMap<String, Package>,
) -> Result<(), Error> {
    crate::cli::print_compiling("Erlang code");

    let escript_path = root.build_path().join("compile_escript.erl");
    let escript_source = std::include_str!("build/compile_escript.erl").to_string();

    crate::fs::write_output(&OutputFile {
        path: escript_path.clone(),
        text: escript_source,
    })?;

    // Run escript to compile Erlang to beam files
    let mut command = process::Command::new("escript");
    command.arg(escript_path);
    command.arg(root.build_path());

    tracing::trace!("Running OS process {:?}", command);
    let status = command.status().map_err(|e| Error::ShellCommand {
        command: "escript".to_string(),
        err: Some(e.kind()),
    })?;

    if status.success() {
        Ok(())
    } else {
        Err(Error::ShellCommand {
            command: "escript".to_string(),
            err: None,
        })
    }
}

fn copy_root_package_to_build(
    root: &ProjectRoot,
    root_config: &PackageConfig,
) -> Result<(), Error> {
    let target = root.default_build_lib_package_path(&root_config.name);
    let path = &root.root;

    // Reset _build dir
    crate::fs::delete_dir(&target)?;
    crate::fs::mkdir(&target)?;

    // Copy source files across
    crate::fs::copy(path.join("gleam.toml"), target.join("gleam.toml"))?;
    crate::fs::copy_dir(path.join("src"), &target)?;
    crate::fs::copy_dir(path.join("test"), &target)?;

    Ok(())
}
