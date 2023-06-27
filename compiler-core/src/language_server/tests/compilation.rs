use crate::language_server::engine::Compilation;

use super::*;

#[test]
fn compile_please() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    let response = engine.compile_please();
    assert!(response.result.is_ok());
    assert!(response.warnings.is_empty());
    assert_eq!(response.compilation, Compilation::Yes(vec![]));

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
    let mut engine = setup_engine(&io);

    io.src_module("app/error", "pub type Error {");

    let response = engine.compile_please();
    assert!(response.result.is_err());
    assert!(response.warnings.is_empty());
    assert_eq!(response.compilation, Compilation::Yes(vec![]));

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
    let mut engine = setup_engine(&io);

    io.test_module("app/error", "pub type Error {");

    let response = engine.compile_please();
    assert!(response.result.is_err());
    assert!(response.warnings.is_empty());
    assert_eq!(response.compilation, Compilation::Yes(vec![]));

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
fn compile_recompile() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    io.src_module("app/src", "pub fn main() { 0 }");

    // The first time it compiles.
    let response = engine.compile_please();
    assert!(response.result.is_ok());
    assert!(response.warnings.is_empty());
    assert_eq!(
        response.compilation,
        Compilation::Yes(vec!["/src/app/src.gleam".into()])
    );

    // The source file has been updated, so the file is compiled again.
    io.src_module("app/src", "pub fn main() { 1 }");
    let response = engine.compile_please();
    assert!(response.result.is_ok());
    assert!(response.warnings.is_empty());
    assert_eq!(
        response.compilation,
        Compilation::Yes(vec!["/src/app/src.gleam".into()])
    );

    // This time it does not compile the module again, instead using the
    // cache from the previous run.
    let response = engine.compile_please();
    assert!(response.result.is_ok());
    assert!(response.warnings.is_empty());
    assert_eq!(response.compilation, Compilation::Yes(vec![]));

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
            // compile_please
            Action::CompilationStarted,
            Action::LockBuild,
            Action::UnlockBuild,
            Action::CompilationFinished,
            // compile_please
            Action::CompilationStarted,
            Action::LockBuild,
            Action::UnlockBuild,
            Action::CompilationFinished,
        ]
    )
}
