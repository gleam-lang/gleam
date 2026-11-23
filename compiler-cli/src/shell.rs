// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2020 The Gleam contributors

use gleam_core::{
    analyse::TargetSupport,
    build::{Codegen, Compile, ErlangOutput, Mode, Options, Runtime, Target},
    error::{Error, ShellCommandFailureReason},
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
            erlang_output: ErlangOutput::Binary,
        },
        crate::build::download_dependencies(paths, crate::cli::Reporter::new())?,
    )?;

    // Don't exit on ctrl+c as it is used by child Erlang shell
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
    let _ = command.status().map_err(|error| Error::ShellCommand {
        program: "erl".into(),
        reason: ShellCommandFailureReason::IoError(error.kind()),
    })?;
    Ok(())
}

pub fn version(target: Target, runtime: Option<Runtime>) -> Result<String, Error> {
    let program = match target {
        Target::Erlang => "erl",
        Target::JavaScript => match runtime.unwrap() {
            Runtime::Bun => "bun",
            Runtime::Deno => "deno",
            Runtime::NodeJs => "node",
        },
    };

    // Prepare the baseline command to be used
    let mut command = Command::new(program);

    // Modify the arguments based on the program type
    let _ = match target {
        Target::Erlang =>  command.arg("-noshell").arg("-eval").arg("{ok, V} = file:read_file(filename:join([code:root_dir(), \"releases\", erlang:system_info(otp_release), \"OTP_VERSION\"])), io:frwrite(V), halt()."),
        Target::JavaScript => match runtime.unwrap() {
            Runtime::Bun =>  command.arg("-p").arg("Bun.version"),
            Runtime::NodeJs => command.arg("-p").arg("process.versions.node"),
            Runtime::Deno => command.arg("eval").arg("-p").arg("Deno.version.deno"),
        },
    };

    // Execute the command and get the output
    tracing::info!("Running OS process {:?}", command);
    let result = command.output().map_err(|error| Error::ShellCommand {
        program: program.to_string(),
        reason: ShellCommandFailureReason::IoError(error.kind()),
    });

    // Finally remap the result into a suitable value
    result.map(|output| String::from_utf8_lossy(&output.stdout).trim().to_string())
}
