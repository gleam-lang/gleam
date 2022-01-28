use gleam_core::{
    build::{Mode, Options, Target},
    error::Error,
    paths,
};
use std::process::Command;

pub fn command() -> Result<(), Error> {
    // Build project
    let _ = crate::build::main(&Options {
        perform_codegen: true,
        mode: Mode::Dev,
        target: Some(Target::Erlang),
    })?;

    // Don't exit on ctrl+c as it is used by child erlang shell
    ctrlc::set_handler(move || {}).expect("Error setting Ctrl-C handler");

    // Prepare the Erlang shell command
    let mut command = Command::new("erl");

    // Print character lists as lists
    let _ = command.arg("-stdlib").arg("shell_strings").arg("false");

    // Specify locations of .beam files
    let packages = paths::build_packages(Mode::Dev, Target::Erlang);
    for entry in crate::fs::read_dir(&packages)?.filter_map(Result::ok) {
        let _ = command.arg("-pa").arg(entry.path().join("ebin"));
    }

    // Start applications
    //let config = crate::config::root_config()?;
    //let package = paths::build_package(Mode::Dev, Target::Erlang, &config.name);
    //let app_file = format!("{}.app", &config.name);
    //let _ = command.arg("-s").arg(package.join("ebin").join(app_file));
    //let _ = command.arg("-s").arg(&config.name);
    //let _ = command.arg("-s").arg("application").arg("ensure_all_started").arg(&config.name).arg("temporary");

    crate::cli::print_running("Erlang shell");

    // Run the shell
    tracing::info!("Running OS process {:?}", command);
    let _ = command.status().map_err(|e| Error::ShellCommand {
        program: "erl".to_string(),
        err: Some(e.kind()),
    })?;
    Ok(())
}
