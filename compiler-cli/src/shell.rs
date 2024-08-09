use gleam_core::{
    analyse::TargetSupport,
    build::{Codegen, Mode, Options, Target},
    error::Error,
};
use std::{process::Command, sync::Arc};

pub fn command() -> Result<(), Error> {
    let paths = crate::find_project_paths()?;

    // Build project
    let _ = crate::build::main(
        Options {
            root_target_support: TargetSupport::Enforced,
            warnings_as_errors: false,
            codegen: Codegen::All,
            mode: Mode::Dev,
            target: Some(Target::Erlang),
        },
        crate::build::download_dependencies()?,
        Arc::new(crate::cli::Reporter::new()),
    )?;

    // Don't exit on ctrl+c as it is used by child erlang shell
    ctrlc::set_handler(move || {}).expect("Error setting Ctrl-C handler");

    // Prepare the Erlang shell command
    let mut command = Command::new("erl");

    // Print character lists as lists
    let _ = command.arg("-stdlib").arg("shell_strings").arg("false");

    // Specify locations of .beam files
    let packages = paths.build_directory_for_target(Mode::Dev, Target::Erlang);
    for entry in crate::fs::read_dir(packages)?.filter_map(Result::ok) {
        let _ = command.arg("-pa").arg(entry.path().join("ebin"));
    }

    crate::cli::print_running("Erlang shell");

    // Run the shell
    tracing::info!("Running OS process {:?}", command);
    let _ = command.status().map_err(|e| Error::ShellCommand {
        program: "erl".into(),
        err: Some(e.kind()),
    })?;
    Ok(())
}
