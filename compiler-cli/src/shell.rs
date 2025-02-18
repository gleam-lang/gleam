use gleam_core::{
    analyse::TargetSupport,
    build::{Codegen, Compile, Mode, Options, Target},
    error::Error,
    paths::ProjectPaths,
};
use std::process::Command;

pub fn command(paths: &ProjectPaths) -> Result<(), Error> {
    // Build project
    let _ = crate::build::main(
        paths,
        Options {
            root_target_support: TargetSupport::Enforced,
            warnings_as_errors: false,
            codegen: Codegen::All,
            compile: Compile::All,
            mode: Mode::Dev,
            target: Some(Target::Erlang),
            no_print_progress: false,
        },
        crate::build::download_dependencies(paths, crate::cli::Reporter::new())?,
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
