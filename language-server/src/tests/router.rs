// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

use std::time::SystemTime;

use gleam_core::{Error, io::FileSystemWriter, paths::ProjectPaths};

use crate::{files::FileSystemProxy, tests::Action};

use super::LanguageServerTestIO;

type Router = crate::router::Router<LanguageServerTestIO, LanguageServerTestIO>;

#[test]
fn recompile_after_no_changes_does_not_redownload_dependencies() {
    let paths = ProjectPaths::new("/app".into());
    let (io, mut router) = set_up_minimal_router(&paths);

    assert_eq!(
        compile(&mut router, &paths),
        Ok(()),
        "Pre-condition: Initial compile should succeed"
    );

    {
        let mut actions = io.actions.lock().unwrap();
        assert!(
            actions.contains(&Action::DownloadDependencies),
            "Expectation: Initial compile should download dependencies"
        );
        actions.clear();
    }

    assert_eq!(
        compile(&mut router, &paths),
        Ok(()),
        "Recompile should succeed"
    );

    {
        let actions = io.actions.lock().unwrap();
        assert!(
            !actions.contains(&Action::DownloadDependencies),
            "Recompile should not re-download dependencies"
        );
    }
}

#[test]
fn deleting_build_dir_redownloads_dependencies() {
    let paths = ProjectPaths::new("/app".into());
    let (io, mut router) = set_up_minimal_router(&paths);

    _ = compile(&mut router, &paths);
    io.actions.lock().unwrap().clear();

    io.delete_directory(&paths.build_directory()).unwrap();
    assert_eq!(
        compile(&mut router, &paths),
        Ok(()),
        "Compile after deleting build directory should succeed"
    );

    {
        let actions = io.actions.lock().unwrap();
        assert!(
            actions.contains(&Action::DownloadDependencies),
            "Compile after deleting build directory should re-download dependencies"
        );
    }
}

#[test]
fn changing_config_redownloads_dependencies() {
    let paths = ProjectPaths::new("/app".into());
    let (io, mut router) = set_up_minimal_router(&paths);

    _ = compile(&mut router, &paths);
    io.actions.lock().unwrap().clear();

    let toml = r#"name = "wobble"
    version = "1.0.0""#;
    io.write(&paths.root_config(), toml).unwrap();
    io.io
        .try_set_modification_time(&paths.root_config(), SystemTime::now())
        .unwrap();

    assert_eq!(
        compile(&mut router, &paths),
        Ok(()),
        "Compile after changing gleam.toml should succeed"
    );

    {
        let actions = io.actions.lock().unwrap();
        assert!(
            actions.contains(&Action::DownloadDependencies),
            "Compile after changing gleam.toml should re-download dependencies"
        );
    }
}

fn compile(router: &mut Router, paths: &ProjectPaths) -> Result<(), Error> {
    router
        .project_for_path(paths.root().into())
        .unwrap()
        .unwrap()
        .engine
        .compile_please()
        .result
}

fn set_up_minimal_router(paths: &ProjectPaths) -> (LanguageServerTestIO, Router) {
    let io = LanguageServerTestIO::new();
    let router = Router::new(io.clone(), FileSystemProxy::new(io.clone()));

    let toml = r#"name = "wibble"
    version = "1.0.0""#;

    io.write(&paths.root_config(), toml).unwrap();
    (io, router)
}

#[test]
fn feedback_survives_engine_recreation() {
    let paths = ProjectPaths::new("/app".into());
    let (io, mut router) = set_up_minimal_router(&paths);
    let path = paths.root().join("src/Invalid.gleam");
    let filename = gleam_core::diagnostic::Diagnostic {
        title: "Invalid module name".into(),
        text: "Invalid module name".into(),
        level: gleam_core::diagnostic::Level::Error,
        location: None,
        hint: None,
    };
    // Filename validation can report diagnostics without creating an engine.
    _ = router
        .feedback_for_path(&path)
        .expect("Test project state is available")
        .update_module_filename(path.clone(), Some(filename));
    assert!(
        io.actions
            .lock()
            .expect("Actions lock is not poisoned")
            .is_empty()
    );
    assert_eq!(compile(&mut router, &paths), Ok(()));
    router.delete_engine_for_path(&path);
    assert_eq!(compile(&mut router, &paths), Ok(()));
    let feedback = router
        .feedback_for_path(&path)
        .expect("Test project state is available")
        .update_module_filename(path.clone(), None);
    // A clearing update is required because the client still has the error.
    assert_eq!(feedback.diagnostics.get(&path), Some(&vec![]));
}

#[test]
fn feedback_is_isolated_between_projects() {
    let paths = ProjectPaths::new("/app".into());
    let (io, mut router) = set_up_minimal_router(&paths);
    let other_paths = ProjectPaths::new("/other".into());
    io.write(&other_paths.root_config(), "")
        .expect("Test project state is available");
    let path = paths.root().join("src/main.gleam");
    let error = Error::Parse {
        path: path.clone(),
        src: "blah".into(),
        error: Box::new(gleam_core::parse::error::ParseError {
            error: gleam_core::parse::error::ParseErrorType::ConcatPatternVariableLeftHandSide,
            location: src_span::SrcSpan::new(1, 4),
        }),
    };
    _ = router
        .feedback_for_path(&path)
        .expect("Test project state is available")
        .error(error);
    assert!(
        router
            .feedback_for_path(other_paths.root())
            .expect("Test project state is available")
            .response(crate::engine::Compilation::Yes(vec![]), vec![])
            .diagnostics
            .is_empty()
    );
    assert_eq!(
        router
            .feedback_for_path(paths.root())
            .expect("Test project state is available")
            .response(crate::engine::Compilation::Yes(vec![]), vec![])
            .diagnostics
            .get(&path),
        Some(&vec![])
    );
}
