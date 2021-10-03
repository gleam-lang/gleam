use gleam_core::{
    build::{project_root::ProjectRoot, Origin},
    error::Error,
    io::OutputFile,
};
use itertools::Itertools;
use std::{path::PathBuf, process::Command};

#[derive(Debug)]
struct EunitFile {
    should_be_compiled: bool,
    path: PathBuf,
    content: String,
}

pub fn command() -> Result<(), Error> {
    let root = ProjectRoot::new();
    let config = crate::config::root_config()?;

    // Build project
    let packages = crate::new_build_main(config)?;

    crate::cli::print_running("eunit");

    // Build a list of test modules
    let test_modules = packages
        .into_iter()
        .flat_map(|(_, p)| p.modules)
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
    let _ = compile_command.arg("-o");
    let _ = compile_command.arg(root.build_path());

    eunit_files
        .iter()
        .filter(|&file| file.should_be_compiled)
        .for_each(|file| {
            let _ = compile_command.arg(file.path.clone());
        });

    tracing::trace!("Running OS process {:?}", compile_command);
    let _ = compile_command.status().map_err(|e| Error::ShellCommand {
        command: "erlc".to_string(),
        err: Some(e.kind()),
    })?;

    // Prepare the escript command for running tests
    let mut command = Command::new("escript");
    let _ = command.arg(root.build_path().join("eunit_runner.erl"));

    let ebin_paths: String = crate::fs::read_dir(root.default_build_lib_path())?
        .filter_map(Result::ok)
        .map(|entry| entry.path().join("ebin").display().to_string())
        .join(",");

    // we supply two parameters to the escript. First is a comma seperated
    let _ = command.arg(ebin_paths);
    let _ = command.arg(test_modules);

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
