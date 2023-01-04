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

mod add;
mod build;
mod build_lock;
mod cli;
mod compile_package;
mod config;
mod dependencies;
mod docs;
mod export;
mod format;
mod fs;
mod hex;
mod http;
mod lsp;
mod new;
mod panic;
mod publish;
mod run;
mod shell;
mod telemetry;

use config::root_config;
use dependencies::UseManifest;
pub use gleam_core::{
    error::{Error, Result},
    warning::Warning,
};

use gleam_core::{
    build::{Mode, Options, Runtime, Target},
    hex::RetirementReason,
};
use hex::ApiKeyCommand as _;

use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};
use strum::VariantNames;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser, Debug)]
#[clap(version)]
enum Command {
    /// Build the project
    Build {
        /// Emit compile time warnings as errors
        #[clap(long)]
        warnings_as_errors: bool,

        /// The platform to target
        #[clap(long, ignore_case = true)]
        target: Option<Target>,
    },

    /// Type check the project
    Check,

    /// Publish the project to the Hex package manager
    ///
    /// This command uses this environment variables:
    ///
    /// - HEXPM_USER: (optional) The Hex username to authenticate with.
    /// - HEXPM_PASS: (optional) The Hex password to authenticate with.
    #[clap(verbatim_doc_comment)]
    Publish {
        #[clap(long)]
        replace: bool,
        #[clap(short, long)]
        yes: bool,
    },

    /// Render HTML documentation
    #[clap(subcommand)]
    Docs(Docs),

    /// Work with dependency packages
    #[clap(subcommand)]
    Deps(Dependencies),

    /// Update dependency packages to their latest versions
    Update,

    /// Work with the Hex package manager
    #[clap(subcommand)]
    Hex(Hex),

    /// Create a new project
    New(NewOptions),

    /// Format source code
    Format {
        /// Files to format
        #[clap(default_value = ".")]
        files: Vec<String>,

        /// Read source from STDIN
        #[clap(long)]
        stdin: bool,

        /// Check if inputs are formatted without changing them
        #[clap(long)]
        check: bool,
    },

    /// Start an Erlang shell
    Shell,

    /// Run the project
    #[clap(trailing_var_arg = true)]
    Run {
        /// The platform to target
        #[clap(long, ignore_case = true)]
        target: Option<Target>,

        #[clap(long, ignore_case = true, possible_values = Runtime::VARIANTS)]
        runtime: Option<Runtime>,

        arguments: Vec<String>,
    },

    /// Run the project tests
    #[clap(trailing_var_arg = true)]
    Test {
        /// The platform to target
        #[clap(long, ignore_case = true)]
        target: Option<Target>,

        #[clap(long, ignore_case = true)]
        runtime: Option<Runtime>,

        arguments: Vec<String>,
    },

    /// Compile a single Gleam package
    #[clap(hide = true)]
    CompilePackage(CompilePackage),

    /// Read and print gleam.toml for debugging
    #[clap(hide = true)]
    PrintConfig,

    /// Add new project dependencies
    Add {
        /// The names of Hex packages to add
        #[clap(required = true)]
        packages: Vec<String>,

        /// Add the packages as dev-only dependencies
        #[clap(long)]
        dev: bool,
    },

    /// Clean build artifacts
    Clean,

    /// Run the language server, to be used by editors
    #[clap(name = "lsp", hide = true)]
    LanguageServer,

    /// Export something useful from the Gleam project
    #[clap(subcommand)]
    Export(ExportTarget),
}

#[derive(Subcommand, Debug, Clone, Copy)]
pub enum ExportTarget {
    /// Precompiled Erlang, suitable for deployment.
    ErlangShipment,
}

#[derive(Args, Debug, Clone)]
pub struct NewOptions {
    /// Location of the project root
    pub project_root: String,

    /// Name of the project
    #[clap(long)]
    pub name: Option<String>,

    /// Description of the project
    #[clap(long, default_value = "A Gleam project")]
    pub description: String,

    #[clap(
        long,
        possible_values = new::Template::VARIANTS,
        ignore_case = true,
        default_value = "lib"
    )]
    pub template: new::Template,

    /// Skip git initialization and creation of .gitignore, .git/* and .github/* files
    #[clap(long)]
    pub skip_git: bool,

    /// Skip creation of .github/* files
    #[clap(long)]
    pub skip_github: bool,
}

#[derive(Args, Debug)]
pub struct CompilePackage {
    /// The compilation target for the generated project
    #[clap(long, ignore_case = true)]
    target: Target,

    /// The directory of the Gleam package
    #[clap(long = "package")]
    package_directory: PathBuf,

    /// A directory to write compiled package to
    #[clap(long = "out")]
    output_directory: PathBuf,

    /// A directories of precompiled Gleam projects
    #[clap(long = "lib")]
    libraries_directory: PathBuf,

    /// Skip Erlang to BEAM bytecode compilation if given
    #[clap(long = "no-beam")]
    skip_beam_compilation: bool,
}

#[derive(Subcommand, Debug)]
enum Dependencies {
    /// List all dependency packages
    List,

    /// Download all dependency packages
    Download,

    /// Update dependency packages to their latest versions
    Update,
}

#[derive(Subcommand, Debug)]
enum Hex {
    /// Retire a release from Hex
    ///
    /// This command uses this environment variables:
    ///
    /// - HEXPM_USER: (optional) The Hex username to authenticate with.
    /// - HEXPM_PASS: (optional) The Hex password to authenticate with.
    #[clap(verbatim_doc_comment)]
    Retire {
        package: String,

        version: String,

        #[clap(possible_values = RetirementReason::VARIANTS)]
        reason: RetirementReason,

        message: Option<String>,
    },

    /// Un-retire a release from Hex
    ///
    /// This command uses this environment variables:
    ///
    /// - HEXPM_USER: (optional) The Hex username to authenticate with.
    /// - HEXPM_PASS: (optional) The Hex password to authenticate with.
    #[clap(verbatim_doc_comment)]
    Unretire { package: String, version: String },
}

#[derive(Subcommand, Debug)]
enum Docs {
    /// Render HTML docs locally
    Build,

    /// Publish HTML docs to HexDocs
    ///
    /// This command uses this environment variables:
    ///
    /// - HEXPM_USER: (optional) The Hex username to authenticate with.
    /// - HEXPM_PASS: (optional) The Hex password to authenticate with.
    #[clap(verbatim_doc_comment)]
    Publish,

    /// Remove HTML docs from HexDocs
    ///
    /// This command uses this environment variables:
    ///
    /// - HEXPM_USER: (optional) The Hex username to authenticate with.
    /// - HEXPM_PASS: (optional) The Hex password to authenticate with.
    #[clap(verbatim_doc_comment)]
    Remove {
        /// The name of the package
        #[clap(long)]
        package: String,

        /// The version of the docs to remove
        #[clap(long)]
        version: String,
    },
}

fn main() {
    initialise_logger();
    panic::add_handler();
    let stderr = cli::stderr_buffer_writer();

    let result = match Command::parse() {
        Command::Build {
            target,
            warnings_as_errors: _,
        } => command_build(target),

        Command::Check => command_check(),

        Command::Docs(Docs::Build) => docs::build(),

        Command::Docs(Docs::Publish) => docs::publish(),

        Command::Docs(Docs::Remove { package, version }) => docs::remove(package, version),

        Command::Format {
            stdin,
            files,
            check,
        } => format::run(stdin, check, files),

        Command::Deps(Dependencies::List) => dependencies::list(),

        Command::Deps(Dependencies::Download) => {
            dependencies::download(cli::Reporter::new(), None, UseManifest::Yes).map(|_| ())
        }

        Command::Deps(Dependencies::Update) => dependencies::update(),

        Command::New(options) => new::create(options, VERSION),

        Command::Shell => shell::command(),

        Command::Run {
            target,
            arguments,
            runtime,
        } => run::command(arguments, target, runtime, run::Which::Src),

        Command::Test {
            target,
            arguments,
            runtime,
        } => run::command(arguments, target, runtime, run::Which::Test),

        Command::CompilePackage(opts) => compile_package::command(opts),

        Command::Publish { replace, yes } => publish::command(replace, yes),

        Command::PrintConfig => print_config(),

        Command::Hex(Hex::Retire {
            package,
            version,
            reason,
            message,
        }) => hex::RetireCommand::new(package, version, reason, message).run(),

        Command::Hex(Hex::Unretire { package, version }) => {
            hex::UnretireCommand::new(package, version).run()
        }

        Command::Add { packages, dev } => add::command(packages, dev),

        Command::Update => dependencies::update(),

        Command::Clean => clean(),

        Command::LanguageServer => lsp::main(),

        Command::Export(ExportTarget::ErlangShipment) => export::erlang_shipment(),
    };

    match result {
        Ok(_) => {
            tracing::info!("Successfully completed");
        }
        Err(error) => {
            tracing::error!(error = ?error, "Failed");
            let mut buffer = stderr.buffer();
            error.pretty(&mut buffer);
            stderr.print(&buffer).expect("Final result error writing");
            std::process::exit(1);
        }
    }
}

fn command_check() -> Result<(), Error> {
    let _ = build::main(Options {
        perform_codegen: false,
        mode: Mode::Dev,
        target: None,
    })?;
    Ok(())
}

fn command_build(target: Option<Target>) -> Result<(), Error> {
    let _ = build::main(Options {
        perform_codegen: true,
        mode: Mode::Dev,
        target,
    })?;
    Ok(())
}

fn print_config() -> Result<()> {
    let config = root_config()?;
    println!("{:#?}", config);
    Ok(())
}

fn clean() -> Result<()> {
    fs::delete_dir(&gleam_core::paths::build())
}

fn initialise_logger() {
    let enable_colours = std::env::var("GLEAM_LOG_NOCOLOUR").is_err();
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_env_filter(std::env::var("GLEAM_LOG").unwrap_or_else(|_| "off".to_string()))
        .with_target(false)
        .with_ansi(enable_colours)
        .without_time()
        .init();
}

fn print_warning(w: &Warning) {
    let buffer_writer = cli::stderr_buffer_writer();
    let mut buffer = buffer_writer.buffer();
    w.pretty(&mut buffer);
    #[allow(clippy::unwrap_used)]
    buffer_writer.print(&buffer).unwrap();
}
