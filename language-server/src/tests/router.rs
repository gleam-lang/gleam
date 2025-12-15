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
