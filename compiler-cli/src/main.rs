#![warn(
    clippy::all,
    clippy::dbg_macro,
    clippy::todo,
    clippy::mem_forget,
    clippy::use_self,
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
    unused_qualifications
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
mod fix;
mod format;
mod fs;
mod hex;
mod http;
mod lsp;
mod new;
mod panic;
mod publish;
mod remove;
mod run;
mod shell;

use config::root_config;
use dependencies::UseManifest;
use fs::{get_current_directory, get_project_root};
pub use gleam_core::error::{Error, Result};

use gleam_core::{
    analyse::TargetSupport,
    build::{Codegen, Mode, Options, Runtime, Target},
    hex::RetirementReason,
    paths::ProjectPaths,
    version::COMPILER_VERSION,
};
use hex::ApiKeyCommand as _;
use std::str::FromStr;
use std::sync::Arc;

use camino::Utf8PathBuf;

use clap::{
    builder::{styling, PossibleValuesParser, Styles, TypedValueParser},
    Args, Parser, Subcommand,
};
use strum::VariantNames;

#[derive(Parser, Debug)]
#[command(
    version,
    next_display_order = None,
    help_template = "\
{before-help}{name} {version}

{usage-heading} {usage}

{all-args}{after-help}",
    styles = Styles::styled()
        .header(styling::AnsiColor::Yellow.on_default())
        .usage(styling::AnsiColor::Yellow.on_default())
        .literal(styling::AnsiColor::Green.on_default())
)]

enum Command {
    /// Build the project
    Build {
        /// Emit compile time warnings as errors
        #[arg(long)]
        warnings_as_errors: bool,

        #[arg(short, long, ignore_case = true, help = target_doc())]
        target: Option<Target>,
    },

    /// Type check the project
    Check {
        #[arg(short, long, ignore_case = true, help = target_doc())]
        target: Option<Target>,
    },

    /// Publish the project to the Hex package manager
    ///
    /// This command uses this environment variables:
    ///
    /// - HEXPM_USER: (optional) The Hex username to authenticate with.
    /// - HEXPM_PASS: (optional) The Hex password to authenticate with.
    /// - HEXPM_API_KEY: (optional) A Hex API key to use instead of authenticating.
    #[command(verbatim_doc_comment)]
    Publish {
        #[arg(long)]
        replace: bool,
        #[arg(short, long)]
        yes: bool,
    },

    /// Render HTML documentation
    #[command(subcommand)]
    Docs(Docs),

    /// Work with dependency packages
    #[command(subcommand)]
    Deps(Dependencies),

    /// Update dependency packages to their latest versions
    Update,

    /// Work with the Hex package manager
    #[command(subcommand)]
    Hex(Hex),

    /// Create a new project
    New(NewOptions),

    /// Format source code
    Format {
        /// Files to format
        #[arg(default_value = ".")]
        files: Vec<String>,

        /// Read source from STDIN
        #[arg(long)]
        stdin: bool,

        /// Check if inputs are formatted without changing them
        #[arg(long)]
        check: bool,
    },
    /// Rewrite deprecated Gleam code
    Fix,

    /// Start an Erlang shell
    Shell,

    /// Run the project
    #[command(trailing_var_arg = true)]
    Run {
        #[arg(short, long, ignore_case = true, help = target_doc())]
        target: Option<Target>,

        #[arg(long, ignore_case = true, help = runtime_doc())]
        runtime: Option<Runtime>,

        /// The module to run
        #[arg(short, long)]
        module: Option<String>,

        /// Don't print progress information
        #[arg(long)]
        no_print_progress: bool,

        arguments: Vec<String>,
    },

    /// Run the project tests
    #[command(trailing_var_arg = true)]
    Test {
        #[arg(short, long, ignore_case = true, help = target_doc())]
        target: Option<Target>,

        #[arg(long, ignore_case = true, help = runtime_doc())]
        runtime: Option<Runtime>,

        arguments: Vec<String>,
    },

    /// Compile a single Gleam package
    #[command(hide = true)]
    CompilePackage(CompilePackage),

    /// Read and print gleam.toml for debugging
    #[command(hide = true)]
    PrintConfig,

    /// Add new project dependencies
    Add {
        /// The names of Hex packages to add
        #[arg(required = true)]
        packages: Vec<String>,

        /// Add the packages as dev-only dependencies
        #[arg(long)]
        dev: bool,
    },

    /// Remove project dependencies
    Remove {
        /// The names of packages to remove
        #[arg(required = true)]
        packages: Vec<String>,
    },

    /// Clean build artifacts
    Clean,

    /// Run the language server, to be used by editors
    #[command(name = "lsp")]
    LanguageServer,

    /// Export something useful from the Gleam project
    #[command(subcommand)]
    Export(ExportTarget),
}

fn target_doc() -> String {
    format!("The platform to target ({})", Target::VARIANTS.join("|"))
}

fn runtime_doc() -> String {
    format!("The runtime to target ({})", Runtime::VARIANTS.join("|"))
}

#[derive(Subcommand, Debug, Clone)]
pub enum ExportTarget {
    /// Precompiled Erlang, suitable for deployment
    ErlangShipment,
    /// The package bundled into a tarball, suitable for publishing to Hex
    HexTarball,
    /// The JavaScript prelude module
    JavascriptPrelude,
    /// The TypeScript prelude module
    TypescriptPrelude,
    /// Information on the modules, functions, and types in the project in JSON format
    PackageInterface {
        #[arg(long = "out", required = true)]
        /// The path to write the JSON file to
        output: Utf8PathBuf,
    },
}

#[derive(Args, Debug, Clone)]
pub struct NewOptions {
    /// Location of the project root
    pub project_root: String,

    /// Name of the project
    #[arg(long)]
    pub name: Option<String>,

    #[arg(long, ignore_case = true, default_value = "lib")]
    pub template: new::Template,

    /// Skip git initialization and creation of .gitignore, .git/* and .github/* files
    #[arg(long)]
    pub skip_git: bool,

    /// Skip creation of .github/* files
    #[arg(long)]
    pub skip_github: bool,
}

#[derive(Args, Debug)]
pub struct CompilePackage {
    /// The compilation target for the generated project
    #[arg(long, ignore_case = true)]
    target: Target,

    /// The directory of the Gleam package
    #[arg(long = "package")]
    package_directory: Utf8PathBuf,

    /// A directory to write compiled package to
    #[arg(long = "out")]
    output_directory: Utf8PathBuf,

    /// A directories of precompiled Gleam projects
    #[arg(long = "lib")]
    libraries_directory: Utf8PathBuf,

    /// The location of the JavaScript prelude module, relative to the `out`
    /// directory.
    ///
    /// Required when compiling to JavaScript.
    ///
    /// This likely wants to be a `.mjs` file as NodeJS does not permit
    /// importing of other JavaScript file extensions.
    ///
    #[arg(long = "javascript-prelude")]
    javascript_prelude: Option<Utf8PathBuf>,

    /// Skip Erlang to BEAM bytecode compilation if given
    #[arg(long = "no-beam")]
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
    /// - HEXPM_API_KEY: (optional) A Hex API key to use instead of authenticating.
    #[command(verbatim_doc_comment)]
    Retire {
        package: String,

        version: String,

        #[arg(value_parser = PossibleValuesParser::new(RetirementReason::VARIANTS).map(|s| RetirementReason::from_str(&s).unwrap()))]
        reason: RetirementReason,

        message: Option<String>,
    },

    /// Un-retire a release from Hex
    ///
    /// This command uses this environment variables:
    ///
    /// - HEXPM_USER: (optional) The Hex username to authenticate with.
    /// - HEXPM_PASS: (optional) The Hex password to authenticate with.
    /// - HEXPM_API_KEY: (optional) A Hex API key to use instead of authenticating.
    #[command(verbatim_doc_comment)]
    Unretire { package: String, version: String },

    /// Revert a release from Hex
    ///
    /// This command uses this environment variables:
    ///
    /// - HEXPM_USER: (optional) The Hex username to authenticate with.
    /// - HEXPM_PASS: (optional) The Hex password to authenticate with.
    /// - HEXPM_API_KEY: (optional) A Hex API key to use instead of authenticating.
    Revert {
        #[arg(long)]
        package: Option<String>,

        #[arg(long)]
        version: Option<String>,
    },
}

#[derive(Subcommand, Debug)]
enum Docs {
    /// Render HTML docs locally
    Build {
        /// Opens the docs in a browser after rendering
        #[arg(long)]
        open: bool,

        #[arg(short, long, ignore_case = true, help = target_doc())]
        target: Option<Target>,
    },

    /// Publish HTML docs to HexDocs
    ///
    /// This command uses this environment variables:
    ///
    /// - HEXPM_USER: (optional) The Hex username to authenticate with.
    /// - HEXPM_PASS: (optional) The Hex password to authenticate with.
    /// - HEXPM_API_KEY: (optional) A Hex API key to use instead of authenticating.
    #[command(verbatim_doc_comment)]
    Publish,

    /// Remove HTML docs from HexDocs
    ///
    /// This command uses this environment variables:
    ///
    /// - HEXPM_USER: (optional) The Hex username to authenticate with.
    /// - HEXPM_PASS: (optional) The Hex password to authenticate with.
    /// - HEXPM_API_KEY: (optional) A Hex API key to use instead of authenticating.
    #[command(verbatim_doc_comment)]
    Remove {
        /// The name of the package
        #[arg(long)]
        package: String,

        /// The version of the docs to remove
        #[arg(long)]
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
            warnings_as_errors,
        } => command_build(target, warnings_as_errors),

        Command::Check { target } => command_check(target),

        Command::Docs(Docs::Build { open, target }) => {
            docs::build(docs::BuildOptions { open, target })
        }

        Command::Docs(Docs::Publish) => docs::publish(),

        Command::Docs(Docs::Remove { package, version }) => docs::remove(package, version),

        Command::Format {
            stdin,
            files,
            check,
        } => format::run(stdin, check, files),

        Command::Fix => fix::run(),

        Command::Deps(Dependencies::List) => dependencies::list(),

        Command::Deps(Dependencies::Download) => download_dependencies(),

        Command::Deps(Dependencies::Update) => dependencies::update(),

        Command::New(options) => new::create(options, COMPILER_VERSION),

        Command::Shell => shell::command(),

        Command::Run {
            target,
            arguments,
            runtime,
            module,
            no_print_progress,
        } => run::command(
            arguments,
            target,
            runtime,
            module,
            no_print_progress,
            run::Which::Src,
        ),

        Command::Test {
            target,
            arguments,
            runtime,
        } => run::command(arguments, target, runtime, None, false, run::Which::Test),

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

        Command::Hex(Hex::Revert { package, version }) => hex::revertcommand(package, version),

        Command::Add { packages, dev } => add::command(packages, dev),

        Command::Remove { packages } => remove::command(packages),

        Command::Update => dependencies::update(),

        Command::Clean => clean(),

        Command::LanguageServer => lsp::main(),

        Command::Export(ExportTarget::ErlangShipment) => export::erlang_shipment(),
        Command::Export(ExportTarget::HexTarball) => export::hex_tarball(),
        Command::Export(ExportTarget::JavascriptPrelude) => export::javascript_prelude(),
        Command::Export(ExportTarget::TypescriptPrelude) => export::typescript_prelude(),
        Command::Export(ExportTarget::PackageInterface { output }) => {
            export::package_interface(output)
        }
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

fn command_check(target: Option<Target>) -> Result<()> {
    let _ = build::main(
        Options {
            root_target_support: TargetSupport::Enforced,
            warnings_as_errors: false,
            codegen: Codegen::DepsOnly,
            mode: Mode::Dev,
            target,
        },
        build::download_dependencies()?,
        Arc::new(cli::Reporter::new()),
    )?;
    Ok(())
}

fn command_build(target: Option<Target>, warnings_as_errors: bool) -> Result<()> {
    let _ = build::main(
        Options {
            root_target_support: TargetSupport::Enforced,
            warnings_as_errors,
            codegen: Codegen::All,
            mode: Mode::Dev,
            target,
        },
        build::download_dependencies()?,
        Arc::new(cli::Reporter::new()),
    )?;
    Ok(())
}

fn print_config() -> Result<()> {
    let config = root_config()?;
    println!("{config:#?}");
    Ok(())
}

fn clean() -> Result<()> {
    let paths = find_project_paths()?;
    fs::delete_directory(&paths.build_directory())
}

fn initialise_logger() {
    let enable_colours = std::env::var("GLEAM_LOG_NOCOLOUR").is_err();
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_env_filter(std::env::var("GLEAM_LOG").unwrap_or_else(|_| "off".into()))
        .with_target(false)
        .with_ansi(enable_colours)
        .without_time()
        .init();
}

fn find_project_paths() -> Result<ProjectPaths> {
    let current_dir = get_current_directory()?;
    get_project_root(current_dir).map(ProjectPaths::new)
}

#[cfg(test)]
fn project_paths_at_current_directory_without_toml() -> ProjectPaths {
    let current_dir = get_current_directory().expect("Failed to get current directory");
    ProjectPaths::new(current_dir)
}

fn download_dependencies() -> Result<()> {
    let paths = find_project_paths()?;
    _ = dependencies::download(&paths, cli::Reporter::new(), None, UseManifest::Yes)?;
    Ok(())
}
