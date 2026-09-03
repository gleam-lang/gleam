// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use camino::Utf8PathBuf;
use gleam_cli::{Command, ExportTarget, fs};
use std::process;

fn package(package: &str) -> Utf8PathBuf {
    Utf8PathBuf::from(&format!("./packages/{package}"))
}

fn escript_compile(package: &str) -> Result<Utf8PathBuf, gleam_core::Error> {
    let working_directory = Utf8PathBuf::from(&format!("./packages/{package}"));
    let escript_path = working_directory.join(package);
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
    let escript = assert_escript_compile("hello_joe");
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
    let escript = assert_escript_compile("with_dependency");
    let status = process::Command::new("escript")
        .arg(escript)
        .status()
        .expect("executable escript");
    assert!(status.success(), "escript should run OK");
}

#[test]
fn escript_without_main_function() {
    let error = escript_compile("without_main_function")
        .expect_err("escripts require a main function")
        .pretty_string();
    insta::assert_snapshot!(error);
}

#[test]
fn escript_with_wrong_arity_main_function() {
    let error = escript_compile("with_wrong_arity_main_function")
        .expect_err("escripts require a main function")
        .pretty_string();
    insta::assert_snapshot!(error);
}

#[test]
fn javascript_prelude() {
    let out = tempfile::NamedTempFile::new().unwrap();
    let path = Utf8PathBuf::from(out.path().as_os_str().to_str().unwrap());
    Command::Export(ExportTarget::JavaScriptPrelude {
        output: Some(path.clone()),
    })
    .run(package("hello_joe"))
    .unwrap();
    let contents = std::fs::read_to_string(path).unwrap();
    assert_eq!(contents, gleam_core::javascript::PRELUDE);
}

#[test]
fn typescript_prelude() {
    let out = tempfile::NamedTempFile::new().unwrap();
    let path = Utf8PathBuf::from(out.path().as_os_str().to_str().unwrap());
    Command::Export(ExportTarget::TypeScriptPrelude {
        output: Some(path.clone()),
    })
    .run(package("hello_joe"))
    .unwrap();
    let contents = std::fs::read_to_string(path).unwrap();
    assert_eq!(contents, gleam_core::javascript::PRELUDE_TS_DEF);
}

#[test]
fn package_information() {
    let out = tempfile::NamedTempFile::new().unwrap();
    let path = Utf8PathBuf::from(out.path().as_os_str().to_str().unwrap());
    Command::Export(ExportTarget::PackageInformation {
        output: Some(path.clone()),
    })
    .run(package("hello_joe"))
    .unwrap();
    let contents = std::fs::read_to_string(path).unwrap();
    insta::assert_snapshot!(contents);
}

#[test]
fn package_interface() {
    let out = tempfile::NamedTempFile::new().unwrap();
    let path = Utf8PathBuf::from(out.path().as_os_str().to_str().unwrap());
    Command::Export(ExportTarget::PackageInterface {
        output: Some(path.clone()),
    })
    .run(package("hello_joe"))
    .unwrap();
    let contents = std::fs::read_to_string(path).unwrap();
    insta::assert_snapshot!(contents);
}
