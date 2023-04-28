use super::common::*;

#[test]
fn compile_please_empty_project() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_language_server(&io);

    let response = engine.compile_please();

    assert!(response.result.is_ok());
    assert!(response.warnings.is_empty());
    assert!(response.compiled_modules.is_empty());

    drop(engine);

    assert_compile_please_actions(
        io,
        vec![
            Action::CompilationStarted,
            Action::LockBuild,
            Action::UnlockBuild,
            Action::CompilationFinished,
        ],
    );
}

#[test]
fn compile_please_error_in_src() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_language_server(&io);

    io.write_source_module("app/error", "pub type Error {");

    let response = engine.compile_please();

    assert!(response.result.is_err());
    assert!(response.warnings.is_empty());
    assert!(response.compiled_modules.is_empty());

    drop(engine);

    assert_compile_please_actions(
        io,
        vec![
            Action::CompilationStarted,
            Action::LockBuild,
            Action::UnlockBuild,
            Action::CompilationFinished,
        ],
    );
}

#[test]
fn compile_please_error_in_test() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_language_server(&io);

    io.write_test_module("app/error", "pub type Error {");

    let response = engine.compile_please();

    assert!(response.result.is_err());
    assert!(response.warnings.is_empty());
    assert!(response.compiled_modules.is_empty());

    drop(engine);

    assert_compile_please_actions(
        io,
        vec![
            Action::CompilationStarted,
            Action::LockBuild,
            Action::UnlockBuild,
            Action::CompilationFinished,
        ],
    );
}

fn assert_compile_please_actions(io: LanguageServerTestIO, expected_actions: Vec<Action>) {
    let actual_actions = io.into_actions();

    let actions_during_new = vec![
        Action::DependencyDownloadingStarted,
        Action::DownloadDependencies,
        Action::DependencyDownloadingFinished,
        Action::LockBuild,
        Action::UnlockBuild,
    ];

    assert_eq!(
        actual_actions,
        [actions_during_new, expected_actions].concat()
    );
}
