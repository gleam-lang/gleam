use super::*;

#[test]
fn compile_sends_correct_actions() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

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
    let mut engine = setup_engine(&io);

    let valid_gleam_program = r#"
        pub fn main() {
            True
        }
    "#;
    io.src_module("app", valid_gleam_program);

    let response = engine.compile_please();

    assert!(response.result.is_ok());
    assert!(response.warnings.is_empty());
    assert_eq!(
        response.compiled_modules,
        vec![std::path::PathBuf::from("/src/app.gleam")]
    );
}

#[test]
fn compile_project_with_warnings() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    let program_with_warnings = r#"
        pub fn main() {}
    "#;
    io.src_module("app", program_with_warnings);

    let response = engine.compile_please();

    assert!(response.result.is_ok());
    assert_eq!(response.warnings.len(), 1);
    assert_eq!(response.compiled_modules.len(), 1);
}

#[test]
fn compile_multiple_files() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    let app_file = r#"
        import app2
        pub fn main() { app2.main() }
    "#;
    let app_2_file = r#"
        pub fn main() { True }
    "#;
    io.src_module("app", app_file);
    io.src_module("app2", app_2_file);

    let response = engine.compile_please();

    assert!(response.result.is_ok());
    assert_eq!(response.compiled_modules.len(), 2);
}

#[test]
fn compile_test_file() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    let app_file = r#"
        import app2
        pub fn main() { app2.main() }
    "#;
    let app_2_file = r#"
        pub fn main() { True }
    "#;
    let app_test_file = r#"
        import app
        pub fn main() { app.main() == True }
    "#;

    io.src_module("app", app_file);
    io.src_module("app2", app_2_file);
    io.test_module("app_test", app_test_file);

    let response = engine.compile_please();

    assert!(response.result.is_ok());
    assert_eq!(response.compiled_modules.len(), 3);
}

#[test]
fn compile_empty_project() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    let response = engine.compile_please();

    assert!(response.result.is_ok());
    assert!(response.warnings.is_empty());
    assert!(response.compiled_modules.is_empty());
}

#[test]
fn compile_error_in_src() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    io.src_module("app/error", "pub type Error {");

    let response = engine.compile_please();

    assert!(response.result.is_err());
    assert!(response.warnings.is_empty());
    assert!(response.compiled_modules.is_empty());

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
fn compile_error_in_test() {
    let io = LanguageServerTestIO::new();
    let mut engine = setup_engine(&io);

    io.test_module("app/error", "pub type Error {");

    let response = engine.compile_please();

    assert!(response.result.is_err());
    assert!(response.warnings.is_empty());
    assert!(response.compiled_modules.is_empty());

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
