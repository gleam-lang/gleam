use gleam_core::{
    build::{Mode, Target},
    error::Error,
    paths,
};
use std::process::Command;

#[derive(Debug, Clone, Copy)]
pub enum Which {
    Src,
    Test,
}

pub fn command(which: Which) -> Result<(), Error> {
    let config = crate::config::root_config()?;

    // Determine which module to run
    let module = match which {
        Which::Src => config.name,
        Which::Test => format!("{}_test", &config.name),
    };

    // Build project so we have bytecode to run
    let _ = crate::build::main()?;

    // Don't exit on ctrl+c as it is used by child erlang shell
    ctrlc::set_handler(move || {}).expect("Error setting Ctrl-C handler");

    // Prepare the Erlang shell command
    let mut command = Command::new("erl");

    // Specify locations of .beam files
    let packages = paths::build_packages(Mode::Dev, Target::Erlang);
    for entry in crate::fs::read_dir(&packages)?.filter_map(Result::ok) {
        let _ = command.arg("-pa").arg(entry.path().join("ebin"));
    }

    // Run the main function.
    let _ = command.arg("-eval");
    let _ = command.arg(&format!("gleam@@main:run({})", &module));

    // Don't run the Erlang shell
    let _ = command.arg("-noshell");

    // Tell the BEAM that any following argument are for the program
    let _ = command.arg("-extra"); // TODO: Pass any user specified command line flags

    crate::cli::print_running(&format!("{}.main", module));

    // Run the shell
    tracing::info!("Running OS process {:?}", command);
    let status = command.status().map_err(|e| Error::ShellCommand {
        command: "erl".to_string(),
        err: Some(e.kind()),
    })?;

    std::process::exit(status.code().unwrap_or_default());
}
