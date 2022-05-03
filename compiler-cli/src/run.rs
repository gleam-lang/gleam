use gleam_core::{
    build::{Mode, Options, Target},
    config::PackageConfig,
    error::Error,
    io::CommandExecutor,
    paths,
};

use crate::fs::ProjectIO;

#[derive(Debug, Clone, Copy)]
pub enum Which {
    Src,
    Test,
}

pub fn command(arguments: Vec<String>, target: Option<Target>, which: Which) -> Result<(), Error> {
    let config = crate::config::root_config()?;

    // Determine which module to run
    let module = match which {
        Which::Src => config.name.to_string(),
        Which::Test => format!("{}_test", &config.name),
    };

    // Build project so we have bytecode to run
    let _ = crate::build::main(Options {
        perform_codegen: true,
        mode: Mode::Dev,
        target,
    })?;

    // Don't exit on ctrl+c as it is used by child erlang shell
    ctrlc::set_handler(move || {}).expect("Error setting Ctrl-C handler");

    crate::cli::print_running(&format!("{}.main", module));

    // Run the command
    let status = match target.unwrap_or(config.target) {
        Target::Erlang => run_erlang(&module, arguments),
        Target::JavaScript => run_javascript(&config, &module, arguments),
    }?;

    std::process::exit(status);
}

fn run_erlang(module: &str, arguments: Vec<String>) -> Result<i32, Error> {
    let mut args = vec![];

    // Specify locations of .beam files
    let packages = paths::build_packages(Mode::Dev, Target::Erlang);

    for entry in crate::fs::read_dir(&packages)?.filter_map(Result::ok) {
        args.push("-pa".into());
        args.push(entry.path().join("ebin").to_string_lossy().into());
    }

    args.push("-eval".into());
    args.push(format!("gleam@@main:run({})", &module));

    // Don't run the Erlang shell
    args.push("-noshell".into());

    // Tell the BEAM that any following argument are for the program
    args.push("-extra".into());
    for argument in arguments.into_iter() {
        args.push(argument);
    }

    ProjectIO::new().exec("erl", &args, &[], None, false)
}

fn run_javascript(
    config: &PackageConfig,
    module: &str,
    arguments: Vec<String>,
) -> Result<i32, Error> {
    let mut args = vec![];

    let module = paths::build_package(Mode::Dev, Target::JavaScript, &config.name)
        .join("dist")
        .join(module);

    // Run the main function.
    args.push("-e".into());
    args.push(format!(
        "import('./{}.mjs').then(module => module.main())",
        module.to_string_lossy()
    ));

    // Tell Node that any following argument are for the program
    args.push("--".into());
    for argument in arguments.into_iter() {
        args.push(argument);
    }

    ProjectIO::new().exec("node", &args, &[], None, false)
}
