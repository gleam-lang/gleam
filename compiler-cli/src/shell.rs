use gleam_core::{
    build::project_root::ProjectRoot,
    error::{Error, GleamExpect},
};
use std::path::PathBuf;
use std::process::Command;

pub fn command(root_string: String) -> Result<(), Error> {
    let root_path = PathBuf::from(root_string);
    let root = ProjectRoot::new(root_path.clone());
    let config = crate::config::root_config(&root)?;

    // Build project
    let _ = super::new_build_main(config, root_path)?;

    // Don't exit on ctrl+c as it is used by child erlang shell
    ctrlc::set_handler(move || {}).gleam_expect("Error setting Ctrl-C handler");

    // Prepare the Erlang shell command
    let mut command = Command::new("erl");

    // Print character lists as lists
    let _ = command.arg("-stdlib").arg("shell_strings").arg("false");

    // Specify locations of .beam files
    for entry in crate::fs::read_dir(root.default_build_lib_path())?.filter_map(Result::ok) {
        let _ = command.arg("-pa").arg(entry.path().join("ebin"));
    }

    crate::cli::print_running("Erlang shell");

    // TODO: pass ctrl-c etc through to the shell process

    // Run the shell
    tracing::trace!("Running OS process {:?}", command);
    let _ = command.status().map_err(|e| Error::ShellCommand {
        command: "erl".to_string(),
        err: Some(e.kind()),
    })?;
    Ok(())
}
