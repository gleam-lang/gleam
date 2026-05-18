use camino::Utf8PathBuf;
use gleam_cli::{Command, ExportTarget, fs};

#[test]
fn escript_success() {
    let working_directory = Utf8PathBuf::from("./cases/escript_ok");
    let escript_path = working_directory.join("escript_ok");

    fs::delete_file(&escript_path).expect("must be able to reset test directory");

    Command::Export(ExportTarget::Escript)
        .run(working_directory.clone())
        .expect("should compile successfully");

    assert!(
        escript_path.exists() && escript_path.is_file(),
        "escript should have been created"
    );

    assert!(
        std::process::Command::new("escript")
            .arg(escript_path)
            .status()
            .expect("should run successfully")
            .success(),
        "escrpt should be runnable"
    )
}

#[test]
fn escript_without_main_function() {
    let working_directory = Utf8PathBuf::from("./cases/escript_without_main_function");
    let error = Command::Export(ExportTarget::Escript)
        .run(working_directory)
        .expect_err("escripts require a main function")
        .pretty_string();
    insta::assert_snapshot!(error)
}

#[test]
fn escript_with_wrong_arity_main_function() {
    let working_directory = Utf8PathBuf::from("./cases/escript_with_wrong_arity_main_function");
    let error = Command::Export(ExportTarget::Escript)
        .run(working_directory)
        .expect_err("escripts require zero arity main function")
        .pretty_string();
    insta::assert_snapshot!(error)
}
