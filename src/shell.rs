use crate::{
    build::{self, project_root::ProjectRoot},
    error::Error,
    file,
};
use std::path::PathBuf;
use std::process::Command;
extern crate ctrlc;

pub fn command(root_string: String) -> Result<(), Error> {
    let root_path = PathBuf::from(root_string);
    let root = ProjectRoot::new(root_path.clone());
    let config = root.root_config()?;

    // Build project
    build::main(config, root_path)?;

    // Don't exit on ctrl+c as it is used by child erlang shell
    ctrlc::set_handler(move || {
        println!("\nIf you would like to exit this shell, please use Ctrl+g to switch to user switch prompt and type `q` to quit");
    })
    .expect("Error setting Ctrl-C handler");

    // Prepare the Erlang shell command
    let mut command = Command::new("erl");

    // Specify locations of .beam files
    for entry in file::read_dir(root.default_build_lib_path())?.filter_map(Result::ok) {
        command.arg("-pa");
        command.arg(entry.path().join("ebin"));
    }

    crate::cli::print_running("erl");

    // Run the shell
    tracing::trace!("Running OS process {:?}", command);
    let status = command.status().map_err(|e| Error::ShellCommand {
        command: "erl".to_string(),
        err: Some(e.kind()),
    })?;

    if status.success() {
        Ok(())
    } else {
        Err(Error::ShellCommand {
            command: "erl".to_string(),
            err: None,
        })
    }
}
