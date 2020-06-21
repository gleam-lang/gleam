#![deny(warnings)]

mod ast;
mod bit_string;
mod build;
mod cli;
mod config;
mod diagnostic;
mod docs;
mod erl;
mod error;
mod eunit;
mod file;
mod format;
mod new;
mod parser;
mod pretty;
mod project;
mod shell;
mod typ;
mod warning;

lalrpop_mod!(
    #[allow(deprecated)]
    #[allow(clippy::all)]
    #[allow(dead_code)]
    #[allow(unused_parens)]
    grammar
);

#[macro_use]
extern crate im;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate lalrpop_util;

#[macro_use]
extern crate lazy_static;

use crate::error::Error;
use std::path::PathBuf;
use structopt::clap::AppSettings;
use structopt::StructOpt;
use strum::VariantNames;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(StructOpt, Debug)]
#[structopt(global_settings = &[AppSettings::ColoredHelp, AppSettings::VersionlessSubcommands])]
enum Command {
    #[structopt(name = "build", about = "Compile a project")]
    Build {
        #[structopt(help = "location of the project root", default_value = ".")]
        project_root: String,
    },

    #[structopt(name = "docs", about = "Render HTML documentation for a project")]
    Docs(Docs),

    #[structopt(name = "new", about = "Create a new project")]
    New {
        #[structopt(help = "name of the project")]
        name: String,

        #[structopt(long = "description", help = "description of the project")]
        description: Option<String>,

        #[structopt(help = "location of the project root")]
        project_root: Option<String>,

        #[structopt(
            long = "template",
            possible_values = &new::Template::VARIANTS,
            case_insensitive = true,
            default_value = "lib"
        )]
        template: new::Template,
    },

    #[structopt(name = "format", about = "Format source code")]
    Format {
        #[structopt(
            help = "files to format",
            conflicts_with = "stdin",
            default_value = "."
        )]
        files: Vec<String>,

        #[structopt(
            help = "read source from standard in",
            long = "stdin",
            conflicts_with = "files"
        )]
        stdin: bool,

        #[structopt(
            help = "check if inputs are formatted without changing them",
            long = "check"
        )]
        check: bool,
    },

    #[structopt(name = "shell", about = "Start an Erlang shell")]
    Shell {
        #[structopt(help = "location of the project root", default_value = ".")]
        project_root: String,
    },

    #[structopt(name = "eunit", about = "Run eunit tests")]
    Eunit {
        #[structopt(help = "location of the project root", default_value = ".")]
        project_root: String,
    },
}

#[derive(StructOpt, Debug)]
enum Docs {
    #[structopt(name = "build", about = "Render HTML docs locally")]
    Build {
        #[structopt(help = "location of the project root", default_value = ".")]
        project_root: String,

        #[structopt(help = "the directory to write the docs to", long = "to")]
        to: Option<String>,
    },

    #[structopt(name = "publish", about = "Publish HTML docs to HexDocs")]
    Publish {
        #[structopt(help = "location of the project root", default_value = ".")]
        project_root: String,

        #[structopt(help = "the version to publish ah", long = "version")]
        version: String,
    },

    #[structopt(name = "remove", about = "Remove HTML docs from HexDocs")]
    Remove {
        #[structopt(help = "the name of the package", long = "package")]
        package: String,

        #[structopt(help = "the version to remove the docs of", long = "version")]
        version: String,
    },
}

fn main() {
    initialise_logger();

    let result = match Command::from_args() {
        Command::Build { project_root } => command_build(project_root),

        Command::Docs(Docs::Build { project_root, to }) => docs::command::build(project_root, to),

        Command::Docs(Docs::Publish {
            project_root,
            version,
        }) => docs::command::publish(project_root, version),

        Command::Docs(Docs::Remove { package, version }) => docs::command::remove(package, version),

        Command::Format {
            stdin,
            files,
            check,
        } => format::command::run(stdin, check, files),

        Command::New {
            name,
            description,
            project_root,
            template,
        } => new::create(template, name, description, project_root, VERSION),

        Command::Shell { project_root } => shell::command(project_root),

        Command::Eunit { project_root } => eunit::command(project_root),
    };

    match result {
        Ok(_) => {
            tracing::info!("Successfully completed");
        }
        Err(error) => {
            tracing::error!(error = ?error, "Failed");
            error.pretty_print();
            std::process::exit(1);
        }
    }
}

fn command_build(root: String) -> Result<(), Error> {
    let root = PathBuf::from(&root);
    let config = config::read_project_config(&root)?;

    // Use new build tool
    if config.tool == config::BuildTool::Gleam {
        return build::main(config, root).map(|_| ());
    }

    // Read and type check project
    let (_config, analysed) = project::read_and_analyse(&root)?;

    // Generate Erlang code
    let output_files = erl::generate_erlang(analysed.as_slice());

    // Reset output directory
    file::delete_dir(&root.join(project::OUTPUT_DIR_NAME))?;

    // Print warnings
    warning::print_all(analysed.as_slice());

    // Delete the gen directory before generating the newly compiled files
    file::write_outputs(output_files.as_slice())?;

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
