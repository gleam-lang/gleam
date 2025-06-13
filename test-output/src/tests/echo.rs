use std::{env, io::Read, process::Stdio};

use camino::{Utf8Path, Utf8PathBuf};
use gleam_core::{
    build::{Runtime, Target},
    io::Command,
    paths::ProjectPaths,
};

use gleam_cli::{
    fs,
    run::{self, Which},
};

fn run_and_produce_pretty_snapshot(
    target: Option<Target>,
    runtime: Option<Runtime>,
    project_directory: Utf8PathBuf,
) -> String {
    let project_root = fs::get_project_root(project_directory).expect("project root");
    let paths = ProjectPaths::new(project_root);

    let output = run_and_capture_output(&paths, "main", target, runtime)
        // Since the echo output's contains a path we will replace the `\` with a `/`
        // so that the snapshot doesn't fail on Windows in CI.
        .replace("src\\", "src/");

    let main_module_content =
        fs::read(paths.src_directory().join("main.gleam")).expect("read main module");

    format!(
        "--- main.gleam ----------------------
{main_module_content}

--- gleam run output ----------------
{output}
"
    )
}

fn run_and_capture_output(
    paths: &ProjectPaths,
    main_module: &str,
    target: Option<Target>,
    runtime: Option<Runtime>,
) -> String {
    fs::delete_directory(&paths.build_directory()).expect("delete build directory content");

    let Command {
        program,
        args,
        env,
        cwd: _,
        stdio: _,
    } = run::setup(
        paths,
        vec![],
        target,
        runtime,
        Some(main_module.into()),
        Which::Src,
        true,
    )
    .expect("run setup");

    let mut process = std::process::Command::new(&program)
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .envs(env.iter().map(|pair| (&pair.0, &pair.1)))
        .current_dir(paths.root())
        .spawn()
        .unwrap_or_else(|e| panic!("Failed to spawn process '{}': {}", &program, &e));

    let mut stderr = process.stderr.take().expect("take stderr");
    let mut output = String::new();
    let _ = stderr.read_to_string(&mut output).expect("read stderr");
    let _ = process.wait().expect("run with no errors");
    output
}

macro_rules! assert_echo {
    ($project_name: expr) => {
        let snapshot_name = snapshot_name(None, None, $project_name);
        insta::allow_duplicates! {
            assert_echo!(&snapshot_name, Some(Target::Erlang), None, $project_name);
            assert_echo!(&snapshot_name, Some(Target::JavaScript), Some(Runtime::Bun), $project_name);
            assert_echo!(&snapshot_name, Some(Target::JavaScript), Some(Runtime::Deno), $project_name);
            assert_echo!(&snapshot_name, Some(Target::JavaScript), Some(Runtime::NodeJs), $project_name);
        }
    };

    ($target: expr, $project_name: expr) => {
        let snapshot_name = snapshot_name(Some($target), None, $project_name);
        match $target {
            Target::JavaScript => insta::allow_duplicates! {
                assert_echo!(&snapshot_name, Some($target), Some(Runtime::Bun), $project_name);
                assert_echo!(&snapshot_name, Some($target), Some(Runtime::Deno), $project_name);
                assert_echo!(&snapshot_name, Some($target), Some(Runtime::NodeJs), $project_name);
            },
            Target::Erlang => {
                assert_echo!(&snapshot_name, Some($target), None, $project_name);
            }
        }
    };

    ($snapshot_name: expr, $target: expr, $runtime: expr, $project_name: expr) => {
        let path = fs::canonicalise(&Utf8Path::new("../test-output/cases").join($project_name))
            .expect("canonicalise path");
        let output = run_and_produce_pretty_snapshot($target, $runtime, path);
        insta::assert_snapshot!($snapshot_name.to_string(), output);
    };
}

fn snapshot_name(target: Option<Target>, runtime: Option<Runtime>, suffix: &str) -> String {
    let prefix = match (target, runtime) {
        (None, None) => "".into(),
        (None, Some(runtime)) => format!("{runtime}-"),
        (Some(target), None) => format!("{target}-"),
        (Some(target), Some(runtime)) => format!("{target}-{runtime}-"),
    };
    format!("{prefix}{suffix}")
}

#[test]
fn echo_bitarray() {
    assert_echo!(Target::JavaScript, "echo_bitarray");
    assert_echo!(Target::Erlang, "echo_bitarray");
}

#[test]
fn echo_bool() {
    assert_echo!("echo_bool");
}

#[test]
fn echo_charlist() {
    assert_echo!("echo_charlist");
}

#[test]
fn echo_custom_type() {
    assert_echo!(Target::Erlang, "echo_custom_type");
    assert_echo!(Target::JavaScript, "echo_custom_type");
}

#[test]
fn echo_dict() {
    assert_echo!("echo_dict");
}

#[test]
fn echo_float() {
    assert_echo!(Target::Erlang, "echo_float");
    assert_echo!(Target::JavaScript, "echo_float");
}

#[test]
fn echo_function() {
    assert_echo!("echo_function");
}

#[test]
fn echo_importing_module_named_inspect() {
    assert_echo!("echo_importing_module_named_inspect");
}

#[test]
fn echo_int() {
    assert_echo!("echo_int");
}

#[test]
fn echo_list() {
    assert_echo!("echo_list");
}

#[test]
fn echo_nil() {
    assert_echo!("echo_nil");
}

#[test]
fn echo_string() {
    assert_echo!("echo_string");
}

#[test]
fn echo_tuple() {
    assert_echo!("echo_tuple");
}

#[test]
fn echo_non_record_atom_tag() {
    assert_echo!(Target::Erlang, "echo_non_record_atom_tag");
}

#[test]
fn echo_circular_reference() {
    assert_echo!(Target::JavaScript, "echo_circular_reference");
}

#[test]
fn echo_singleton() {
    assert_echo!("echo_singleton");
}
