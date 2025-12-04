use std::{env, io::Read, process::Stdio};

use camino::{Utf8Path, Utf8PathBuf};
use gleam_core::{
    build::{Runtime, Target}, io::Command, paths::ProjectPaths
};

use gleam_cli::{
    fs,
    run::{self, Which},
};

fn run_and_produce_pretty_snapshot(
    target: Option<Target>,
    module : &str,
    path_to_module : &str,
    runtime: Option<Runtime>,
    project_directory: Utf8PathBuf,
) -> String {
    let project_root = fs::get_project_root(project_directory).expect("project root");
    let paths = ProjectPaths::new(project_root);

    let (output, err_output) = run_and_capture_output(&paths, module, target, runtime);
    
        // Since the echo output's contains a path we will replace the `\` with a `/`
        // so that the snapshot doesn't fail on Windows in CI.
    let (output, err_output) = (output, err_output.replace("src\\", "src/"));
    
    let module_content =
        fs::read(paths.root().join(format!("{path_to_module}/{module}.gleam"))).expect("read module");

    format!(
        "--- {module}.gleam ----------------------\n\
        {module_content}\n\
        \n\
        --- gleam run output ----------------\n\
        {output}\
        --- gleam run err output ------------\n\
        {err_output}"
    )
}

fn run_and_capture_output(
    paths: &ProjectPaths,
    main_module: &str,
    target: Option<Target>,
    runtime: Option<Runtime>,
) -> (String, String) {
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
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .envs(env.iter().map(|pair| (&pair.0, &pair.1)))
        .current_dir(paths.root())
        .spawn()
        .unwrap_or_else(|e| panic!("Failed to spawn process '{}': {}", &program, &e));

    // Write stdout output to output
    let mut stdout = process.stdout.take().expect("read stdout");
    let mut output = String::new();
    let _ = stdout.read_to_string(&mut output).expect("read stdout");
    let _ = process.wait().expect("run with no errors");
    
    // Write stderr output to output
    let mut stderr = process.stderr.take().expect("take stderr");
    let mut err_output = String::new();
    let _ = stderr.read_to_string(&mut err_output).expect("read stderr");
    let _ = process.wait().expect("run with no errors");
    (output, err_output)
}

macro_rules! assert_output {
    ($project_name: expr) => {
        let snapshot_name = snapshot_name(None, None, $project_name);
        insta::allow_duplicates! {
            assert_output!(&snapshot_name, Some(Target::Erlang), None, $project_name);
            assert_output!(&snapshot_name, Some(Target::JavaScript), Some(Runtime::Bun), $project_name);
            assert_output!(&snapshot_name, Some(Target::JavaScript), Some(Runtime::Deno), $project_name);
            assert_output!(&snapshot_name, Some(Target::JavaScript), Some(Runtime::NodeJs), $project_name);
        }
    };

    ($target: expr, $project_name: expr) => {
        let snapshot_name = snapshot_name(Some($target), None, $project_name);
        match $target {
            Target::JavaScript => insta::allow_duplicates! {
                assert_output!(&snapshot_name, Some($target), Some(Runtime::Bun), $project_name);
                assert_output!(&snapshot_name, Some($target), Some(Runtime::Deno), $project_name);
                assert_output!(&snapshot_name, Some($target), Some(Runtime::NodeJs), $project_name);
            },
            Target::Erlang => {
                assert_output!(&snapshot_name, Some($target), None, $project_name);
            }
        }
    };

    ($module_name : expr, $path_to_module: expr, $project_name: expr) => {
        let snapshot_name = snapshot_name(Some(Target::Erlang), None, $project_name);
        assert_output!(&snapshot_name, $module_name, $path_to_module, Some(Target::Erlang), None, $project_name);
    };

    ($snapshot_name: expr, $target: expr, $runtime: expr, $project_name: expr) => {
        assert_output!($snapshot_name, "main", "src", $target, $runtime, $project_name)
    };

    ($snapshot_name: expr, $module_name : expr, $path_to_module: expr, $target: expr, $runtime: expr, $project_name: expr) => {
        let path = fs::canonicalise(&Utf8Path::new("../test-output/cases").join($project_name))
            .expect("canonicalise path");
        let output = run_and_produce_pretty_snapshot($target, $module_name, $path_to_module, $runtime, path);
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
    assert_output!(Target::JavaScript, "echo_bitarray");
    assert_output!(Target::Erlang, "echo_bitarray");
}

#[test]
fn echo_bool() {
    assert_output!("echo_bool");
}

#[test]
fn echo_charlist() {
    assert_output!("echo_charlist");
}

#[test]
fn echo_custom_type() {
    assert_output!(Target::Erlang, "echo_custom_type");
    assert_output!(Target::JavaScript, "echo_custom_type");
}

#[test]
fn echo_dict() {
    assert_output!("echo_dict");
}

#[test]
fn echo_float() {
    assert_output!(Target::Erlang, "echo_float");
    assert_output!(Target::JavaScript, "echo_float");
}

#[test]
fn echo_function() {
    assert_output!("echo_function");
}

#[test]
fn echo_importing_module_named_inspect() {
    assert_output!("echo_importing_module_named_inspect");
}

#[test]
fn echo_int() {
    assert_output!("echo_int");
}

#[test]
fn echo_list() {
    assert_output!("echo_list");
}

#[test]
fn echo_nil() {
    assert_output!("echo_nil");
}

#[test]
fn echo_string() {
    assert_output!("echo_string");
}

#[test]
fn echo_tuple() {
    assert_output!("echo_tuple");
}

#[test]
fn echo_non_record_atom_tag() {
    assert_output!(Target::Erlang, "echo_non_record_atom_tag");
}

#[test]
fn echo_circular_reference() {
    assert_output!(Target::JavaScript, "echo_circular_reference");
}

#[test]
fn echo_singleton() {
    assert_output!("echo_singleton");
}

#[test]
fn echo_with_message() {
    assert_output!("echo_with_message");
}

#[test]
fn otp_test() {
    assert_output!("external_dependency", "external_dependency/src", "otp_root_run_dependency");
}