use super::*;

#[test]
fn compile_please() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_language_server(&io);

    let response = engine.compile_please();
    assert!(response.result.is_ok());
    assert!(response.warnings.is_empty());
    assert!(response.compiled_modules.is_empty());

    drop(engine);
    let actions = io.into_actions();
    assert_eq!(
        actions,
        vec![
            // new
            Action::DependencyDownloadingStarted,
            Action::DownloadDependencies,
            Action::DependencyDownloadingFinished,
            Action::LockBuild,
            Action::UnlockBuild,
            // compile_please
            Action::CompilationStarted,
            Action::LockBuild,
            Action::UnlockBuild,
            Action::CompilationFinished,
        ]
    )
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

    drop(engine);
    let actions = io.into_actions();
    assert_eq!(
        actions,
        vec![
            // new
            Action::DependencyDownloadingStarted,
            Action::DownloadDependencies,
            Action::DependencyDownloadingFinished,
            Action::LockBuild,
            Action::UnlockBuild,
            // compile_please
            Action::CompilationStarted,
            Action::LockBuild,
            Action::UnlockBuild,
            Action::CompilationFinished,
        ]
    )
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

    drop(engine);
    let actions = io.into_actions();
    assert_eq!(
        actions,
        vec![
            // new
            Action::DependencyDownloadingStarted,
            Action::DownloadDependencies,
            Action::DependencyDownloadingFinished,
            Action::LockBuild,
            Action::UnlockBuild,
            // compile_please
            Action::CompilationStarted,
            Action::LockBuild,
            Action::UnlockBuild,
            Action::CompilationFinished,
        ]
    )
}
