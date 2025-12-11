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
    unexpected_cfgs,
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
#![allow(
    clippy::match_single_binding,
    clippy::inconsistent_struct_constructor,
    clippy::assign_op_pattern,
    clippy::len_without_is_empty
)]

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

mod add;
mod beam_compiler;
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
pub mod fs;
mod hex;
mod http;
mod lsp;
mod new;
mod owner;
mod panic;
mod publish;
mod remove;
pub mod run;
mod shell;
mod text_layout;

use config::root_config;
use fs::{get_current_directory, get_project_root};
pub use gleam_core::error::{Error, Result};

use gleam_core::{
    analyse::TargetSupport,
    build::{Codegen, Compile, Mode, NullTelemetry, Options, Runtime, Target},
    hex::RetirementReason,
    paths::ProjectPaths,
    version::COMPILER_VERSION,
};
use std::str::FromStr;

use camino::Utf8PathBuf;

use clap::{
    Args, Parser, Subcommand,
    builder::{PossibleValuesParser, Styles, TypedValueParser, styling},
};
use strum::VariantNames;

#[derive(Args, Debug, Clone)]
struct UpdateOptions {
    /// (optional) Names of the packages to update
    /// If omitted, all dependencies will be updated
    #[arg(verbatim_doc_comment)]
    packages: Vec<String>,
}

#[derive(Args, Debug, Clone)]
struct TreeOptions {
    /// Name of the package to get the dependency tree for
    #[arg(
        short,
        long,
        ignore_case = true,
        conflicts_with = "invert",
        help = "Package to be used as the root of the tree"
    )]
    package: Option<String>,
    /// Name of the package to get the inverted dependency tree for
    #[arg(
        short,
        long,
        ignore_case = true,
        conflicts_with = "package",
        help = "Invert the tree direction and focus on the given package",
        value_name = "PACKAGE"
    )]
    invert: Option<String>,
}

#[derive(Parser, Debug)]
#[command(
    version,
    name = "gleam",
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

        /// Don't print progress information
        #[clap(long)]
        no_print_progress: bool,
    },

    /// Type check the project
    Check {
        #[arg(short, long, ignore_case = true, help = target_doc())]
        target: Option<Target>,
    },

    /// Publish the project to the Hex package manager
    ///
    /// This command uses the environment variable:
    ///
    /// - HEXPM_API_KEY: (optional) A Hex API key to authenticate with the Hex package manager.
    ///
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
    Update(UpdateOptions),

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
    ///
    /// This command runs the `main` function from the `<PROJECT_NAME>` module.
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
        #[clap(long)]
        no_print_progress: bool,

        arguments: Vec<String>,
    },

    /// Run the project tests
    ///
    /// This command runs the `main` function from the `<PROJECT_NAME>_test` module.
    #[command(trailing_var_arg = true)]
    Test {
        #[arg(short, long, ignore_case = true, help = target_doc())]
        target: Option<Target>,

        #[arg(long, ignore_case = true, help = runtime_doc())]
        runtime: Option<Runtime>,

        arguments: Vec<String>,
    },

    /// Run the project development entrypoint
    ///
    /// This command runs the `main` function from the `<PROJECT_NAME>_dev` module.
    #[command(trailing_var_arg = true)]
    Dev {
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

fn template_doc() -> &'static str {
    "The template to use"
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
    /// Package information (gleam.toml) in JSON format
    PackageInformation {
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

    #[arg(long, ignore_case = true, default_value = "erlang", help = template_doc())]
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
    #[arg(verbatim_doc_comment, long = "javascript-prelude")]
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

    /// List all outdated dependencies
    Outdated,

    /// Update dependency packages to their latest versions
    Update(UpdateOptions),

    /// Tree of all the dependency packages
    Tree(TreeOptions),
}

#[derive(Subcommand, Debug)]
enum Hex {
    /// Retire a release from Hex
    ///
    /// This command uses the environment variable:
    ///
    /// - HEXPM_API_KEY: (optional) A Hex API key to authenticate with the Hex package manager.
    ///
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
    /// This command uses this environment variable:
    ///
    /// - HEXPM_API_KEY: (optional) A Hex API key to authenticate with the Hex package manager.
    ///
    #[command(verbatim_doc_comment)]
    Unretire { package: String, version: String },

    /// Revert a release from Hex
    ///
    /// This command uses this environment variable:
    ///
    /// - HEXPM_API_KEY: (optional) A Hex API key to authenticate with the Hex package manager.
    ///
    #[command(verbatim_doc_comment)]
    Revert {
        #[arg(long)]
        package: Option<String>,

        #[arg(long)]
        version: Option<String>,
    },

    /// Deal with package ownership
    #[command(subcommand)]
    Owner(Owner),

    /// Authenticate with Hex
    Authenticate,
}

#[derive(Subcommand, Debug)]
enum Owner {
    /// Transfers ownership of the given package to a new Hex user
    ///
    /// This command uses this environment variable:
    ///
    /// - HEXPM_API_KEY: (optional) A Hex API key to authenticate against the Hex package manager.
    ///
    #[command(verbatim_doc_comment)]
    Transfer {
        package: String,

        /// The username or email of the new owner
        #[arg(long = "to")]
        username_or_email: String,
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
    /// This command uses this environment variable:
    ///
    /// - HEXPM_API_KEY: (optional) A Hex API key to authenticate with the Hex package manager.
    ///
    #[command(verbatim_doc_comment)]
    Publish,

    /// Remove HTML docs from HexDocs
    ///
    /// This command uses this environment variable:
    ///
    /// - HEXPM_API_KEY: (optional) A Hex API key to authenticate with the Hex package manager.
    ///
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

pub fn main() {
    initialise_logger();
    panic::add_handler();
    let stderr = cli::stderr_buffer_writer();
    let result = parse_and_run_command();
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

fn parse_and_run_command() -> Result<(), Error> {
    match Command::parse() {
        Command::Build {
            target,
            warnings_as_errors,
            no_print_progress,
        } => {
            let paths = find_project_paths()?;
            command_build(&paths, target, warnings_as_errors, no_print_progress)
        }

        Command::Check { target } => {
            let paths = find_project_paths()?;
            command_check(&paths, target)
        }

        Command::Docs(Docs::Build { open, target }) => {
            let paths = find_project_paths()?;
            docs::build(&paths, docs::BuildOptions { open, target })
        }

        Command::Docs(Docs::Publish) => {
            let paths = find_project_paths()?;
            docs::publish(&paths)
        }

        Command::Docs(Docs::Remove { package, version }) => docs::remove(package, version),

        Command::Format {
            stdin,
            files,
            check,
        } => format::run(stdin, check, files),

        Command::Fix => {
            let paths = find_project_paths()?;
            fix::run(&paths)
        }

        Command::Deps(Dependencies::List) => {
            let paths = find_project_paths()?;
            dependencies::list(&paths)
        }

        Command::Deps(Dependencies::Download) => {
            let paths = find_project_paths()?;
            download_dependencies(&paths)
        }

        Command::Deps(Dependencies::Outdated) => {
            let paths = find_project_paths()?;
            dependencies::outdated(&paths)
        }

        Command::Deps(Dependencies::Update(options)) => {
            let paths = find_project_paths()?;
            dependencies::update(&paths, options.packages)
        }

        Command::Deps(Dependencies::Tree(options)) => {
            let paths = find_project_paths()?;
            dependencies::tree(&paths, options)
        }

        Command::Hex(Hex::Authenticate) => hex::authenticate(),

        Command::New(options) => new::create(options, COMPILER_VERSION),

        Command::Shell => {
            let paths = find_project_paths()?;
            shell::command(&paths)
        }

        Command::Run {
            target,
            arguments,
            runtime,
            module,
            no_print_progress,
        } => {
            let paths = find_project_paths()?;
            run::command(
                &paths,
                arguments,
                target,
                runtime,
                module,
                run::Which::Src,
                no_print_progress,
            )
        }

        Command::Test {
            target,
            arguments,
            runtime,
        } => {
            let paths = find_project_paths()?;
            run::command(
                &paths,
                arguments,
                target,
                runtime,
                None,
                run::Which::Test,
                false,
            )
        }

        Command::Dev {
            target,
            arguments,
            runtime,
        } => {
            let paths = find_project_paths()?;
            run::command(
                &paths,
                arguments,
                target,
                runtime,
                None,
                run::Which::Dev,
                false,
            )
        }

        Command::CompilePackage(opts) => compile_package::command(opts),

        Command::Publish { replace, yes } => {
            let paths = find_project_paths()?;
            publish::command(&paths, replace, yes)
        }

        Command::PrintConfig => {
            let paths = find_project_paths()?;
            print_config(&paths)
        }

        Command::Hex(Hex::Retire {
            package,
            version,
            reason,
            message,
        }) => hex::retire(package, version, reason, message),

        Command::Hex(Hex::Unretire { package, version }) => hex::unretire(package, version),

        Command::Hex(Hex::Revert { package, version }) => {
            let paths = find_project_paths()?;
            hex::revert(&paths, package, version)
        }

        Command::Hex(Hex::Owner(Owner::Transfer {
            package,
            username_or_email,
        })) => owner::transfer(package, username_or_email),

        Command::Add { packages, dev } => {
            let paths = find_project_paths()?;
            add::command(&paths, packages, dev)
        }

        Command::Remove { packages } => {
            let paths = find_project_paths()?;
            remove::command(&paths, packages)
        }

        Command::Update(options) => {
            let paths = find_project_paths()?;
            dependencies::update(&paths, options.packages)
        }

        Command::Clean => {
            let paths = find_project_paths()?;
            clean(&paths)
        }

        Command::LanguageServer => lsp::main(),

        Command::Export(ExportTarget::ErlangShipment) => {
            let paths = find_project_paths()?;
            export::erlang_shipment(&paths)
        }
        Command::Export(ExportTarget::HexTarball) => {
            let paths = find_project_paths()?;
            export::hex_tarball(&paths)
        }
        Command::Export(ExportTarget::JavascriptPrelude) => export::javascript_prelude(),
        Command::Export(ExportTarget::TypescriptPrelude) => export::typescript_prelude(),
        Command::Export(ExportTarget::PackageInterface { output }) => {
            let paths = find_project_paths()?;
            export::package_interface(&paths, output)
        }
        Command::Export(ExportTarget::PackageInformation { output }) => {
            let paths = find_project_paths()?;
            export::package_information(&paths, output)
        }
    }
}

fn command_check(paths: &ProjectPaths, target: Option<Target>) -> Result<()> {
    let _ = build::main(
        paths,
        Options {
            root_target_support: TargetSupport::Enforced,
            warnings_as_errors: false,
            codegen: Codegen::DepsOnly,
            compile: Compile::All,
            mode: Mode::Dev,
            target,
            no_print_progress: false,
        },
        build::download_dependencies(paths, cli::Reporter::new())?,
    )?;
    Ok(())
}

fn command_build(
    paths: &ProjectPaths,
    target: Option<Target>,
    warnings_as_errors: bool,
    no_print_progress: bool,
) -> Result<()> {
    let manifest = if no_print_progress {
        build::download_dependencies(paths, NullTelemetry)?
    } else {
        build::download_dependencies(paths, cli::Reporter::new())?
    };
    let _ = build::main(
        paths,
        Options {
            root_target_support: TargetSupport::Enforced,
            warnings_as_errors,
            codegen: Codegen::All,
            compile: Compile::All,
            mode: Mode::Dev,
            target,
            no_print_progress,
        },
        manifest,
    )?;
    Ok(())
}

fn print_config(paths: &ProjectPaths) -> Result<()> {
    let config = root_config(paths)?;
    println!("{config:#?}");
    Ok(())
}

fn clean(paths: &ProjectPaths) -> Result<()> {
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

fn download_dependencies(paths: &ProjectPaths) -> Result<()> {
    _ = dependencies::resolve_and_download(
        paths,
        cli::Reporter::new(),
        None,
        Vec::new(),
        dependencies::DependencyManagerConfig {
            use_manifest: dependencies::UseManifest::Yes,
            check_major_versions: dependencies::CheckMajorVersions::No,
        },
    )?;
    Ok(())
}
