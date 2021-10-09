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
    let code = match which {
        Which::Src => format!("{}", &config.name),
        Which::Test => format!("{}_test", &config.name),
    };

    // Build project
    let _ = super::new_build_main()?;

    // Don't exit on ctrl+c as it is used by child erlang shell
    ctrlc::set_handler(move || {}).expect("Error setting Ctrl-C handler");

    // Prepare the Erlang shell command
    let mut command = Command::new("erl");

    // Specify locations of .beam files
    let packages = paths::build_packages(Mode::Dev, Target::Erlang);
    for entry in crate::fs::read_dir(&packages)?.filter_map(Result::ok) {
        let _ = command.arg("-pa").arg(entry.path().join("ebin"));
    }

    // Run the main function
    let _ = command.arg("-noshell");
    let _ = command.arg("-eval");
    let _ = command.arg(format!("{}:main(),erlang:halt()", &code));

    crate::cli::print_running(&code);

    // Run the shell
    tracing::trace!("Running OS process {:?}", command);
    let _ = command.status().map_err(|e| Error::ShellCommand {
        command: "erl".to_string(),
        err: Some(e.kind()),
    })?;
    Ok(())
}
