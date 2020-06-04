use crate::{
    build::{self, project_root::ProjectRoot},
    error::Error,
    file,
};
use std::path::PathBuf;
use std::process::Command;

// TODO: error handling
// TODO: correct handling of ctrl+c
pub fn command(root_string: String) -> Result<(), Error> {
    let root_path = PathBuf::from(root_string);
    let root = ProjectRoot::new(root_path.clone());
    let config = root.root_config()?;

    // Build project
    build::main(config, root_path)?;

    // Prepare the Erlang shell command
    let mut command = Command::new("erl");

    // Specify locations of .beam files
    for entry in file::read_dir(root.default_build_lib_path())?.filter_map(Result::ok) {
        command.arg("-pa");
        command.arg(entry.path().join("ebin"));
    }

    // Run the shell
    tracing::debug!("Running OS process {:?}", command);
    command.spawn().unwrap().wait().unwrap();

    Ok(())
}
