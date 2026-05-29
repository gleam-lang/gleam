use camino::Utf8PathBuf;
use gleam_cli::{Command, ExportTarget, fs};
use std::process;

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
