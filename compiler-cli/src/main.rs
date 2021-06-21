#![warn(
    clippy::all,
    clippy::doc_markdown,
    clippy::dbg_macro,
    clippy::todo,
    clippy::mem_forget,
    // TODO: enable once the false positive bug is solved
    // clippy::use_self,
    clippy::filter_map_next,
    clippy::needless_continue,
    clippy::needless_borrow,
    clippy::match_wildcard_for_single_variants,
    clippy::mismatched_target_os,
    clippy::match_on_vec_items,
    clippy::imprecise_flops,
    clippy::suboptimal_flops,
    clippy::lossy_float_literal,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::fn_params_excessive_bools,
    clippy::inefficient_to_string,
    clippy::linkedlist,
    clippy::macro_use_imports,
    clippy::option_option,
    clippy::verbose_file_reads,
    clippy::unnested_or_patterns,
    rust_2018_idioms,
    missing_debug_implementations,
    missing_copy_implementations,
    trivial_casts,
    trivial_numeric_casts,
    nonstandard_style,
    unused_import_braces,
    unused_qualifications,
)]
#![deny(
    clippy::await_holding_lock,
    clippy::expect_used,
    clippy::if_let_mutex,
    clippy::indexing_slicing,
    clippy::mem_forget,
    clippy::ok_expect,
    clippy::unimplemented,
    clippy::unwrap_used,
    unsafe_code,
    unstable_features,
    unused_results
)]
#![allow(clippy::match_single_binding, clippy::inconsistent_struct_constructor)]

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

mod cli;
mod compile_package;
mod config;
mod docs;
mod eunit;
mod format;
mod fs;
mod new;
mod project;
mod shell;

pub use gleam_core::{
    error::{Error, GleamExpect, Result},
    warning::Warning,
};

use gleam_core::{
    build::{package_compiler, project_root::ProjectRoot, Package, ProjectCompiler, Target},
    config::PackageConfig,
    io::OutputFile,
    project::Analysed,
};

use std::{collections::HashMap, path::PathBuf, process};
use structopt::{clap::AppSettings, StructOpt};
use strum::VariantNames;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(StructOpt, Debug)]
#[structopt(global_settings = &[AppSettings::ColoredHelp, AppSettings::VersionlessSubcommands])]
enum Command {
    #[structopt(
        name = "build",
        about = "Compile a project",
        setting = AppSettings::Hidden,
    )]
    Build {
        #[structopt(help = "location of the project root", default_value = ".")]
        project_root: String,
        #[structopt(
            help = "Emit compile time warnings as errors",
            long = "warnings-as-errors"
        )]
        warnings_as_errors: bool,
    },

    #[structopt(name = "docs", about = "Render HTML documentation")]
    Docs(Docs),

    #[structopt(name = "new", about = "Create a new project")]
    New(NewOptions),

    #[structopt(name = "format", about = "Format source code")]
    Format {
        #[structopt(help = "files to format", default_value = ".")]
        files: Vec<String>,

        #[structopt(help = "read source from standard in", long = "stdin")]
        stdin: bool,

        #[structopt(
            help = "check if inputs are formatted without changing them",
            long = "check"
        )]
        check: bool,
    },

    #[structopt(
        name = "shell",
        about = "Start an Erlang shell",
        setting = AppSettings::Hidden,
    )]
    Shell {
        #[structopt(help = "location of the project root", default_value = ".")]
        project_root: String,
    },

    #[structopt(
        name = "eunit",
        about = "Run eunit tests",
        setting = AppSettings::Hidden,
    )]
    Eunit {
        #[structopt(help = "location of the project root", default_value = ".")]
        project_root: String,
    },

    #[structopt(
        name = "compile-package",
        about = "Compile a single Gleam package",
        setting = AppSettings::Hidden,
    )]
    CompilePackage(CompilePackage),
}

#[derive(StructOpt, Debug, Clone)]
#[structopt(flatten)]
pub struct NewOptions {
    #[structopt(help = "location of the project root")]
    pub project_root: String,

    #[structopt(long, help = "name of the project")]
    pub name: Option<String>,

    #[structopt(
        long = "description",
        help = "description of the project",
        default_value = "A Gleam project"
    )]
    pub description: String,

    #[structopt(
        long = "template",
        possible_values = &new::Template::VARIANTS,
        case_insensitive = true,
        default_value = "lib"
    )]
    pub template: new::Template,
}

#[derive(StructOpt, Debug)]
#[structopt(flatten)]
pub struct CompilePackage {
    #[structopt(
        help = "The compilation target for the generated project",
        long = "target",
        case_insensitive = true,
        default_value = "erlang"
    )]
    target: Target,

    #[structopt(help = "The name of the package being compiled", long = "name")]
    package_name: String,

    #[structopt(help = "A directory of source Gleam code", long = "src")]
    src_directory: PathBuf,

    #[structopt(help = "A directory of test Gleam code", long = "test")]
    test_directory: Option<PathBuf>,

    #[structopt(help = "A directory to write compiled code to", long = "out")]
    output_directory: PathBuf,

    #[structopt(help = "A path to a compiled dependency library", long = "lib")]
    libraries: Vec<PathBuf>,
}

impl CompilePackage {
    pub fn into_package_compiler_options(self) -> package_compiler::Options {
        package_compiler::Options {
            target: self.target,
            name: self.package_name,
            src_path: self.src_directory,
            test_path: self.test_directory,
            out_path: self.output_directory,
        }
    }
}

#[derive(StructOpt, Debug)]
enum Docs {
    #[structopt(name = "build", about = "Render HTML docs locally")]
    Build {
        #[structopt(help = "location of the project root", default_value = ".")]
        project_root: String,

        #[structopt(help = "the directory to write the docs to", long = "to")]
        to: Option<String>,

        #[structopt(help = "the version to publish", long = "version")]
        version: String,
    },

    #[structopt(name = "publish", about = "Publish HTML docs to HexDocs")]
    Publish {
        #[structopt(help = "location of the project root", default_value = ".")]
        project_root: String,

        #[structopt(help = "the version to publish", long = "version")]
        version: String,
    },

    #[structopt(name = "remove", about = "Remove HTML docs from HexDocs")]
    Remove {
        #[structopt(help = "the name of the package", long = "package")]
        package: String,

        #[structopt(help = "the version of the docs to remove", long = "version")]
        version: String,
    },
}

fn main() {
    initialise_logger();

    let result = match Command::from_args() {
        Command::Build {
            project_root,
            warnings_as_errors,
        } => command_build(project_root, warnings_as_errors),

        Command::Docs(Docs::Build {
            project_root,
            version,
            to,
        }) => docs::build(project_root, version, to),

        Command::Docs(Docs::Publish {
            project_root,
            version,
        }) => docs::publish(project_root, version),

        Command::Docs(Docs::Remove { package, version }) => docs::remove(package, version),

        Command::Format {
            stdin,
            files,
            check,
        } => format::run(stdin, check, files),

        Command::New(options) => new::create(options, VERSION),

        Command::Shell { project_root } => shell::command(project_root),

        Command::Eunit { project_root } => eunit::command(project_root),

        Command::CompilePackage(opts) => compile_package::command(opts),
    };

    match result {
        Ok(_) => {
            tracing::info!("Successfully completed");
        }
        Err(error) => {
            tracing::error!(error = ?error, "Failed");
            let buffer_writer = cli::stderr_buffer_writer();
            let mut buffer = buffer_writer.buffer();
            error.pretty(&mut buffer);
            buffer_writer
                .print(&buffer)
                .gleam_expect("Final result error writing");
            std::process::exit(1);
        }
    }
}

fn command_build(root: String, warnings_as_errors: bool) -> Result<(), Error> {
    let root = PathBuf::from(&root);
    let config = config::read_project_config(&root)?;

    // Use new build tool
    if config.tool == gleam_core::config::BuildTool::Gleam {
        return new_build_main(config, root).map(|_| ());
    }

    // Read and type check project
    let (_config, analysed) = project::read_and_analyse(&root)?;

    // Generate Erlang code
    let output_files = gleam_core::erl::generate_erlang(&analysed);

    // Print warnings
    let warning_count = print_warnings(&analysed);

    // Exit if warnings_as_errors and warnings
    if warnings_as_errors && warning_count > 0 {
        return Err(Error::ForbiddenWarnings {
            count: warning_count,
        });
    }

    // Reset output directory
    fs::delete_dir(&root.join(project::OUTPUT_DIR_NAME))?;

    // Delete the gen directory before generating the newly compiled files
    fs::write_outputs(&output_files)?;

    println!("Done!");

    Ok(())
}

fn initialise_logger() {
    tracing_subscriber::fmt()
        .with_env_filter(&std::env::var("GLEAM_LOG").unwrap_or_else(|_| "off".to_string()))
        .with_target(false)
        .without_time()
        .init();
}

pub fn new_build_main(
    root_config: PackageConfig,
    path: PathBuf,
) -> Result<HashMap<String, Package>, Error> {
    let root = ProjectRoot::new(path);
    let telemetry = Box::new(cli::Reporter::new());
    let io = fs::FileSystemAccessor::new();

    tracing::info!("Copying root package to _build");
    copy_root_package_to_build(&root, &root_config)?;

    tracing::info!("Reading package configs from _build");
    let configs = config::package_configs(&root, &root_config.name)?;

    tracing::info!("Compiling packages");
    let packages = ProjectCompiler::new(&root, root_config, configs, telemetry, io).compile()?;

    tracing::info!("Compiling Erlang source code to BEAM bytecode");
    compile_erlang_to_beam(&root)?;

    Ok(packages)
}

fn compile_erlang_to_beam(root: &ProjectRoot) -> Result<(), Error> {
    crate::cli::print_compiling("Erlang code");

    let escript_path = root.build_path().join("compile_escript.erl");
    let escript_source = std::include_str!("build/compile_escript.erl").to_string();

    crate::fs::write_output(&OutputFile {
        path: escript_path.clone(),
        text: escript_source,
    })?;

    // Run escript to compile Erlang to beam files
    let mut command = process::Command::new("escript");
    let _ = command.arg(escript_path);
    let _ = command.arg(root.build_path());

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

fn print_warnings(analysed: &[Analysed]) -> usize {
    analysed
        .iter()
        .flat_map(|a| &a.warnings)
        .inspect(|w| print_warning(w))
        .count()
}

fn print_warning(w: &Warning) {
    let buffer_writer = cli::stderr_buffer_writer();
    let mut buffer = buffer_writer.buffer();
    w.pretty(&mut buffer);
    #[allow(clippy::unwrap_used)]
    buffer_writer.print(&buffer).unwrap();
}
