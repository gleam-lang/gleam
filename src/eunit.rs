use crate::{
    build::{self, project_root::ProjectRoot, Origin},
    error::Error,
    fs::OutputFile,
};
use itertools::Itertools;
use std::{path::PathBuf, process::Command};

#[derive(Debug)]
struct EunitFile {
    should_be_compiled: bool,
    path: PathBuf,
    content: String,
}

pub fn command(root_string: String) -> Result<(), Error> {
    let root_path = PathBuf::from(root_string);
    let root = ProjectRoot::new(root_path.clone());
    let config = root.root_config()?;

    // Build project
    let packages = build::main(config, root_path)?;

    crate::cli::print_running("eunit");

    // Build a list of test modules
    let test_modules = packages
        .into_iter()
        .flat_map(|(_, p)| p.modules.into_iter())
        .filter(|m| m.origin == Origin::Test)
        .map(|m| m.name.replace("/", "@"))
        .join(",");

    // Prepare eunit runner and its dependencies.
    let eunit_files = vec![
        EunitFile {
            should_be_compiled: true,
            path: root.build_path().join("eunit_progress.erl"),
            content: std::include_str!("eunit/eunit_progress.erl").to_string(),
        },
        EunitFile {
            should_be_compiled: false,
            path: root.build_path().join("eunit_runner.erl"),
            content: std::include_str!("eunit/eunit_runner.erl").to_string(),
        },
    ];

    eunit_files.iter().try_for_each(|file| {
        crate::fs::write_output(&OutputFile {
            path: file.path.clone(),
            text: file.content.to_owned(),
        })
    })?;

    // compile eunit runner dependencies in the build path
    let mut compile_command = Command::new("erlc");
    compile_command.arg("-o");
    compile_command.arg(root.build_path());

    eunit_files
        .iter()
        .filter(|&file| file.should_be_compiled)
        .for_each(|file| {
            compile_command.arg(file.path.clone());
        });

    tracing::trace!("Running OS process {:?}", compile_command);
    compile_command.status().map_err(|e| Error::ShellCommand {
        command: "erlc".to_string(),
        err: Some(e.kind()),
    })?;

    // Prepare the escript command for running tests
    let mut command = Command::new("escript");
    command.arg(root.build_path().join("eunit_runner.erl"));

    let ebin_paths: String = crate::fs::read_dir(root.default_build_lib_path())?
        .filter_map(Result::ok)
        .map(|entry| entry.path().join("ebin").as_path().display().to_string())
        .join(",");

    // we supply two parameters to the escript. First is a comma seperated
    command.arg(ebin_paths);
    command.arg(test_modules);

    // Run the shell
    tracing::trace!("Running OS process {:?}", command);
    let status = command.status().map_err(|e| Error::ShellCommand {
        command: "escript".to_string(),
        err: Some(e.kind()),
    })?;

    if status.success() {
        Ok(())
    } else {
        Err(Error::ShellCommand {
            command: "escript".to_string(),
            err: None,
        })
    }
}
