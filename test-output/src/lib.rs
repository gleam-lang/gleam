#[cfg(test)]
mod generated_tests;

use camino::Utf8PathBuf;
use std::{
    fs,
    io::Read,
    process::{Command, Stdio},
};

pub fn prepare(path: &str) -> String {
    let root = Utf8PathBuf::from(path).canonicalize_utf8().unwrap();
    let code = fs::read_to_string(root.as_path().join("src").join("main.gleam")).expect("main");
    let _ = fs::remove_dir_all(root.as_path().join("build"));

    let mut cmd = gleam_run(&root)
        .arg("-terl")
        .spawn()
        .expect("gleam run -terl");

    let mut output = format!("--- CODE\n{code}\n--- OUTPUT ON ERLANG TARGET\n");

    cmd.stderr
        .take()
        .expect("erl stderr")
        .read_to_string(&mut output)
        .expect("erl stderr to string");

    output.push_str("\n--- OUTPUT ON JAVASCRIPT TARGET\n");

    // "cargo run --quiet -- run --module=main --no-print-progress"
    let mut cmd = gleam_run(&root)
        .arg("-tjs")
        .spawn()
        .expect("gleam run -tjs");

    cmd.stderr
        .take()
        .expect("js stderr")
        .read_to_string(&mut output)
        .expect("js stderr to string");

    output
}

fn gleam_run(project_dir: &Utf8PathBuf) -> Command {
    let mut command = Command::new("cargo");

    command
        .arg("run")
        .arg("--quiet")
        // We're running in the `cases` directory so we have to get back three
        // levels to the root of the project to run the compiler.
        .arg("--manifest-path")
        .arg("../../../Cargo.toml")
        .arg("--")
        .arg("run")
        .arg("--module")
        .arg("main")
        .arg("--no-print-progress")
        .current_dir(project_dir)
        .stderr(Stdio::piped());

    command
}
