use super::common::*;

#[test]
fn compile_sends_correct_actions() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_language_server(&io);

    let _response = engine.compile_please();
    drop(engine);

    let actions_during_new = vec![
        Action::DependencyDownloadingStarted,
        Action::DownloadDependencies,
        Action::DependencyDownloadingFinished,
        Action::LockBuild,
        Action::UnlockBuild,
    ];
    let actions_during_compile = vec![
        Action::CompilationStarted,
        Action::LockBuild,
        Action::UnlockBuild,
        Action::CompilationFinished,
    ];

    let actual_actions = io.into_actions();
    assert_eq!(
        actual_actions,
        [actions_during_new, actions_during_compile].concat()
    );
}

#[test]
fn compile_project_successfully() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_language_server(&io);

    let valid_gleam_program = r#"
        pub fn main() {
            True
        }
    "#;
    io.write_source_module("app", valid_gleam_program);

    let response = engine.compile_please();

    assert!(response.result.is_ok());
    assert!(response.warnings.is_empty());
    assert_eq!(
        response.compiled_modules,
        vec![std::path::PathBuf::from("/src/app.gleam")]
    );
}

#[test]
fn compile_empty_project() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_language_server(&io);

    let response = engine.compile_please();

    assert!(response.result.is_ok());
    assert!(response.warnings.is_empty());
    assert!(response.compiled_modules.is_empty());
}

#[test]
fn compile_error_in_src() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_language_server(&io);

    io.write_source_module("app/error", "pub type Error {");

    let response = engine.compile_please();

    assert!(response.result.is_err());
    assert!(response.warnings.is_empty());
    assert!(response.compiled_modules.is_empty());
}

#[test]
fn compile_error_in_test() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_language_server(&io);

    io.write_test_module("app/error", "pub type Error {");

    let response = engine.compile_please();

    assert!(response.result.is_err());
    assert!(response.warnings.is_empty());
    assert!(response.compiled_modules.is_empty());
}
