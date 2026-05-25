use camino::Utf8PathBuf;
use gleam_cli::{Command, CompilePackage, ExportTarget, fs};
use gleam_core::build::Target;
use std::process;
use strum::IntoEnumIterator;

fn escript_compile(case: &str) -> Result<Utf8PathBuf, gleam_core::Error> {
    let working_directory = Utf8PathBuf::from(&format!("./cases/{case}"));
    let escript_path = working_directory.join(case);
    fs::delete_file(&escript_path)
        .and(fs::delete_file(&escript_path.with_extension("cmd")))
        .expect("must be able to reset test directory");

    Command::Export(ExportTarget::Escript)
        .run(working_directory.clone())
        .map(|_| escript_path)
}

fn assert_escript_compile(case: &str) -> Utf8PathBuf {
    let escript_path = escript_compile(case).expect("should compile successfully");
    assert!(
        escript_path.exists() && escript_path.is_file(),
        "escript should have been created"
    );
    escript_path
}

fn make_hextarball(case: &str) -> Result<Utf8PathBuf, gleam_cli::Error> {
    let working_directory = Utf8PathBuf::from(&format!("./cases/{case}"));
    let tarball_path = working_directory.join("build").join("hextarball-0.1.0.tar");
    fs::delete_file(&tarball_path).expect("must be able to reset test directory");

    Command::Export(ExportTarget::HexTarball)
        .run(working_directory)
        .map(|_| tarball_path)
}

fn assert_make_hextarball(case: &str) -> Utf8PathBuf {
    let tarball_path = make_hextarball(case).expect("should make hex tarball successfully");
    assert!(
        tarball_path.exists() && tarball_path.is_file(),
        "hex tarball should have been created"
    );
    tarball_path
}

fn build(case: &str, target: Option<Target>) -> Result<Utf8PathBuf, gleam_cli::Error> {
    let working_directory = Utf8PathBuf::from(&format!("./cases/{case}"));
    let build_directory = working_directory.join("build");
    fs::delete_directory(&build_directory).expect("must be able to reset test directory");

    Command::Build {
        warnings_as_errors: false,
        target,
        no_print_progress: false,
    }
    .run(working_directory)
    .map(|_| build_directory)
}

fn export_erlang_shipment(case: &str) -> Result<Utf8PathBuf, gleam_cli::Error> {
    let working_directory = Utf8PathBuf::from(&format!("./cases/{case}"));
    let erlang_shipment_path = working_directory.join("build").join("erlang-shipment");
    fs::delete_directory(&working_directory.join("build"))
        .expect("must be able to reset test directory");

    Command::Export(ExportTarget::ErlangShipment)
        .run(working_directory)
        .map(|_| erlang_shipment_path)
}

/// All directories, except `javascript_prelude` that is reltive to `output_directory`, are reltive to case directory
fn compile_package(
    case: &str,
    target: Target,
    javascript_prelude: Option<Utf8PathBuf>,
    package_directory: Utf8PathBuf,
    libraries_directory: Utf8PathBuf,
    output_directory: Utf8PathBuf,
) -> Result<Utf8PathBuf, gleam_cli::Error> {
    let working_directory = Utf8PathBuf::from(&format!("./cases/{case}"));

    let package_directory = working_directory.join(package_directory);
    let libraries_directory = working_directory.join(libraries_directory);
    let output_directory = working_directory.join(output_directory);

    fs::delete_directory(&output_directory).expect("must be able to reset test directory");

    Command::CompilePackage(CompilePackage {
        javascript_prelude,
        target,
        package_directory,
        libraries_directory,
        output_directory: output_directory.clone(),
        skip_beam_compilation: false,
    })
    .run(working_directory)
    .map(|_| output_directory)
}

fn publish(case: &str) -> Result<(), gleam_cli::Error> {
    let working_directory = Utf8PathBuf::from(&format!("./cases/{case}"));
    fs::delete_directory(&working_directory.join("build"))
        .expect("should be able to reset test directory");

    Command::Publish {
        replace: false,
        yes: false,
    }
    .run(working_directory)
}

#[test]
fn escript_success() {
    let escript = assert_escript_compile("escript_ok");
    let status = process::Command::new("escript")
        .arg(&escript)
        .status()
        .expect("executable escript");
    assert!(status.success(), "escript should run OK");

    let cmd = escript.with_extension("cmd");
    if cfg!(windows) {
        assert!(cmd.exists(), "*.cmd should exist on Windows");
        let code = std::fs::read_to_string(cmd).expect("read cmd file");
        let expected = "@echo off\r\nescript.exe \"%~dpn0\" %*\r\n";
        assert_eq!(code, expected, "cmd wrapper should run the escript");
    } else {
        assert!(!cmd.exists(), "{cmd} should only exist on Windows");
    }
}

#[test]
fn escript_success_with_dependency() {
    let escript = assert_escript_compile("escript_with_dependency");
    let status = process::Command::new("escript")
        .arg(escript)
        .status()
        .expect("executable escript");
    assert!(status.success(), "escript should run OK");
}

#[test]
fn escript_without_main_function() {
    let error = escript_compile("escript_without_main_function")
        .expect_err("escripts require a main function")
        .pretty_string();
    insta::assert_snapshot!(error);
}

#[test]
fn escript_with_wrong_arity_main_function() {
    let error = escript_compile("escript_with_wrong_arity_main_function")
        .expect_err("escripts require a main function")
        .pretty_string();
    insta::assert_snapshot!(error);
}

#[test]
fn hextarball() {
    assert_make_hextarball("hextarball");
}

#[test]
fn build_unicode_path() {
    for target in Target::iter() {
        build("unicode_path ⭐", Some(target)).expect("should build successfully");
    }
}

#[test]
fn javascipt_prelude() {
    let working_directory = Utf8PathBuf::from("./cases/javascript_prelude");
    let prelude_template_path = Utf8PathBuf::from("../compiler-core/templates/prelude.mjs");
    let prelude_path = working_directory.join("prelude.mjs");
    let main_path = working_directory.join("main.mjs");
    fs::copy(prelude_template_path, &prelude_path).expect("must be able to copy prelude");

    let status = process::Command::new("node")
        .arg(main_path)
        .status()
        .expect("executable node");
    assert!(status.success(), "node should run OK");

    fs::delete_file(&prelude_path).expect("must be able to delete file");
}

#[test]
fn erlang_shipment_no_dev_deps() {
    let erlang_shipment_path = export_erlang_shipment("erlang_shipment_no_dev_deps")
        .expect("should export erlang shipment successfully");

    assert!(
        erlang_shipment_path.exists() && erlang_shipment_path.is_dir(),
        "erlang shipment should have been created"
    );

    let gleam_stdlib_path = erlang_shipment_path.join("gleam_stdlib");
    assert!(
        gleam_stdlib_path.exists() && gleam_stdlib_path.is_dir(),
        "gleam_stdlib should be in the shipment"
    );

    let hpack_path = erlang_shipment_path.join("hpack");
    assert!(
        hpack_path.exists() && hpack_path.is_dir(),
        "hpack from hpack_erl should be in the shipment"
    );

    let gleeunit_path = erlang_shipment_path.join("gleeunit");
    assert!(
        !gleeunit_path.exists(),
        "test dependency gleeunit should not be in the shipment"
    );

    let root_path = erlang_shipment_path.join("shipment_test");
    assert!(
        root_path.exists() && root_path.is_dir(),
        "root package shipment_test should be in the shipment"
    );
}

#[test]
fn compile_package0() {
    let output_directory = compile_package(
        "compile_package0",
        Target::Erlang,
        None,
        Utf8PathBuf::from("."),
        Utf8PathBuf::from("."),
        Utf8PathBuf::from("out"),
    )
    .expect("should compile successfully");
    let status = process::Command::new("erl")
        .args([
            "-pa",
            &format!("{}/ebin", output_directory),
            "-noshell",
            "-eval",
            "erlang:display(two:main()),erlang:display(three:test_()),halt()",
        ])
        .status()
        .expect("should run successfully");
    assert!(status.success(), "erl should run OK");
}

#[test]
fn compile_package1() {
    let first_output_directory = compile_package(
        "compile_package1",
        Target::Erlang,
        None,
        Utf8PathBuf::from("app1"),
        Utf8PathBuf::from("."),
        Utf8PathBuf::from("out1"),
    )
    .expect("should compile successfully");

    let second_output_directory = compile_package(
        "compile_package1",
        Target::Erlang,
        None,
        Utf8PathBuf::from("app2"),
        Utf8PathBuf::from("."),
        Utf8PathBuf::from("out2"),
    )
    .expect("should compile successfully");

    let status = process::Command::new("erl")
        .args([
            "-pa",
            &format!("{}/ebin", first_output_directory),
            &format!("{}/ebin", second_output_directory),
            "-noshell",
            "-eval",
            "erlang:display(two:main()),halt()",
        ])
        .status()
        .expect("should run successfully");
    assert!(status.success(), "erl should run OK");
}

#[test]
fn publishing_default_main() {
    let error = publish("publishing_default_main")
        .expect_err("should not publish anything")
        .pretty_string();
    insta::assert_snapshot!(error);
}

#[test]
fn publishing_default_readme() {
    let error = publish("publishing_default_readme")
        .expect_err("should not publish anything")
        .pretty_string();
    insta::assert_snapshot!(error);
}
