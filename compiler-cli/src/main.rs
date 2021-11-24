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
mod cli;
mod compile_package;
mod config;
mod dependencies;
mod docs;
mod format;
mod fs;
mod hex;
mod http;
mod new;
mod panic;
mod project;
mod publish;
mod run;
mod shell;

use config::root_config;
pub use gleam_core::{
    error::{Error, Result},
    warning::Warning,
};

use gleam_core::{
    build::{package_compiler, Target},
    diagnostic::{self, Severity},
    error::wrap,
    hex::RetirementReason,
    project::Analysed,
};
use hex::ApiKeyCommand as _;

use std::{
    io::Write,
    path::{Path, PathBuf},
};
use structopt::{clap::AppSettings, StructOpt};
use strum::VariantNames;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(StructOpt, Debug)]
#[structopt(global_settings = &[AppSettings::ColoredHelp, AppSettings::VersionlessSubcommands])]
enum Command {
    /// Build the project
    Build {
        /// Emit compile time warnings as errors
        #[structopt(long)]
        warnings_as_errors: bool,
    },

    /// Publish the project to the Hex package manager
    Publish,

    /// Render HTML documentation
    Docs(Docs),

    /// Work with dependency packages
    Deps(Dependencies),

    /// Work with the Hex package manager
    Hex(Hex),

    /// Create a new project
    New(NewOptions),

    /// Format source code
    Format {
        /// Files to format
        #[structopt(default_value = ".")]
        files: Vec<String>,

        /// Read source from STDIN
        #[structopt(long)]
        stdin: bool,

        /// Check if inputs are formatted without changing them
        #[structopt(long)]
        check: bool,
    },

    /// Start an Erlang shell
    Shell,

    /// Run the project
    #[structopt(settings = &[AppSettings::TrailingVarArg])]
    Run { arguments: Vec<String> },

    /// Run the project tests
    #[structopt(settings = &[AppSettings::TrailingVarArg])]
    Test { arguments: Vec<String> },

    /// Compile a single Gleam package
    #[structopt(setting = AppSettings::Hidden)]
    CompilePackage(CompilePackage),

    /// Read and print gleam.toml for debugging
    #[structopt(setting = AppSettings::Hidden)]
    PrintConfig,

    /// Add a new Hex package as a project dependency
    Add {
        package: String,

        #[structopt(long)]
        dev: bool,
    },
}

#[derive(StructOpt, Debug, Clone)]
#[structopt(flatten)]
pub struct NewOptions {
    /// Location of the project root
    pub project_root: String,

    /// Name of the project
    #[structopt(long)]
    pub name: Option<String>,

    /// Description of the project
    #[structopt(long, default_value = "A Gleam project")]
    pub description: String,

    #[structopt(
        long,
        possible_values = &new::Template::VARIANTS,
        case_insensitive = true,
        default_value = "lib"
    )]
    pub template: new::Template,
}

#[derive(StructOpt, Debug)]
#[structopt(flatten)]
pub struct CompilePackage {
    /// The compilation target for the generated project
    #[structopt(long, case_insensitive = true, default_value = "erlang")]
    target: Target,

    /// The name of the package being compiler
    #[structopt(long = "name")]
    package_name: String,

    /// A directory of source Gleam code
    #[structopt(long = "src")]
    src_directory: PathBuf,

    /// A directory of test Gleam code
    #[structopt(long = "test")]
    test_directory: Option<PathBuf>,

    /// A directory to write compiled code to
    #[structopt(long = "out")]
    output_directory: PathBuf,

    /// A path to a compiled dependency library
    #[structopt(long = "lib")]
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
            write_metadata: true,
        }
    }
}

#[derive(StructOpt, Debug)]
enum Dependencies {
    /// Download packages to the local cache
    Download,
}

#[derive(StructOpt, Debug)]
enum Hex {
    /// Retire a release from Hex
    Retire {
        package: String,

        version: String,

        #[structopt(possible_values = &RetirementReason::VARIANTS)]
        reason: RetirementReason,

        message: Option<String>,
    },

    /// Un-retire a release from Hex
    Unretire { package: String, version: String },
}

#[derive(StructOpt, Debug)]
enum Docs {
    /// Render HTML docs locally
    Build,

    /// Publish HTML docs to HexDocs
    Publish,

    /// Remove HTML docs from HexDocs
    Remove {
        /// The name of the package
        #[structopt(long)]
        package: String,

        /// The version of the docs to remove
        #[structopt(long)]
        version: String,
    },
}

fn main() {
    initialise_logger();
    panic::add_handler();
    let stderr = cli::stderr_buffer_writer();

    let result = match Command::from_args() {
        Command::Build { warnings_as_errors } => command_build(&stderr, warnings_as_errors),

        Command::Docs(Docs::Build) => docs::build(),

        Command::Docs(Docs::Publish) => docs::PublishCommand::publish(),

        Command::Docs(Docs::Remove { package, version }) => docs::remove(package, version),

        Command::Format {
            stdin,
            files,
            check,
        } => format::run(stdin, check, files),

        Command::Deps(Dependencies::Download) => dependencies::download().map(|_| ()),

        Command::New(options) => new::create(options, VERSION),

        Command::Shell => shell::command(),

        Command::Run { arguments } => run::command(&arguments, run::Which::Src),

        Command::Test { arguments } => run::command(&arguments, run::Which::Test),

        Command::CompilePackage(opts) => compile_package::command(opts),

        Command::Publish => publish::command(),

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

        Command::Add { package, dev } => add::command(package, dev),
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

const REBAR_DEPRECATION_NOTICE: &str = "The built-in rebar3 support is deprecated and will \
be removed in a future version of Gleam.

Please switch to the new Gleam build tool or update your project to use the new `gleam \
compile-package` API with your existing build tool.

";

fn command_build(stderr: &termcolor::BufferWriter, warnings_as_errors: bool) -> Result<(), Error> {
    let mut buffer = stderr.buffer();
    let root = Path::new("./");

    // Use new build tool if not in a rebar or mix project
    if !root.join("rebar.config").exists() && !root.join("mix.exs").exists() {
        return build::main().map(|_| ());
    }

    diagnostic::write_title(
        &mut buffer,
        "Deprecated rebar3 build command",
        Severity::Warning,
    );
    buffer
        .write_all(wrap(REBAR_DEPRECATION_NOTICE).as_bytes())
        .expect("rebar deprecation message");
    buffer.flush().expect("flush");
    stderr
        .print(&buffer)
        .expect("command_build_rebar_deprecated_write");

    // Read and type check project
    let (_config, analysed) = project::read_and_analyse(&root)?;

    // Generate Erlang code
    let output_files = gleam_core::erlang::generate_erlang(&analysed);

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

fn print_config() -> Result<()> {
    let config = root_config()?;
    println!("{:#?}", config);
    Ok(())
}

fn initialise_logger() {
    tracing_subscriber::fmt()
        .with_env_filter(&std::env::var("GLEAM_LOG").unwrap_or_else(|_| "off".to_string()))
        .with_target(false)
        .without_time()
        .init();
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
