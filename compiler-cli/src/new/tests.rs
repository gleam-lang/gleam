use std::path::PathBuf;

use camino::Utf8PathBuf;
use gleam_core::Error;

#[test]
fn new() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.path().join("my_project")).expect("Non Utf8 Path");

    let creator = super::Creator::new(
        super::NewOptions {
            project_root: path.to_string(),
            template: super::Template::Erlang,
            name: None,
            skip_git: false,
            skip_github: false,
        },
        "1.0.0-gleam",
    )
    .unwrap();

    creator.run().unwrap();

    assert!(path.join(".git").exists());
    assert!(path.join("README.md").exists());
    assert!(path.join("gleam.toml").exists());
    assert!(path.join("src/my_project.gleam").exists());
    assert!(path.join("test/my_project_test.gleam").exists());
    assert!(path.join(".github/workflows/test.yml").exists());

    let toml = crate::fs::read(path.join("gleam.toml")).unwrap();
    assert!(toml.contains("name = \"my_project\""));
}

#[test]
fn new_with_default_template() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.into_path()).expect("Non Utf8 Path");

    let creator = super::Creator::new(
        super::NewOptions {
            project_root: path.join("my_project").to_string(),
            template: super::Template::Erlang,
            name: None,
            skip_git: false,
            skip_github: true,
        },
        "1.0.0-gleam",
    )
    .unwrap();
    creator.run().unwrap();

    insta::glob!(&path, "my_project/[^.]**/*.*", |file_path| {
        if !file_path.is_dir() {
            insta::assert_snapshot!(
                crate::fs::read(
                    Utf8PathBuf::from_path_buf(file_path.to_path_buf()).expect("Non Utf8 Path"),
                )
                .unwrap()
            );
        }
    });
}

#[test]
fn new_with_javascript_template() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.into_path()).expect("Non Utf8 Path");

    let creator = super::Creator::new(
        super::NewOptions {
            project_root: path.join("my_project").to_string(),
            template: super::Template::JavaScript,
            name: None,
            skip_git: false,
            skip_github: true,
        },
        "1.0.0-gleam",
    )
    .unwrap();
    creator.run().unwrap();

    insta::glob!(&path, "my_project/[^.]**/*.*", |file_path| {
        if !file_path.is_dir() {
            insta::assert_snapshot!(
                crate::fs::read(
                    Utf8PathBuf::from_path_buf(file_path.to_path_buf()).expect("Non Utf8 Path"),
                )
                .unwrap()
            );
        }
    });
}

#[test]
fn new_with_skip_git() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.path().join("my_project")).expect("Non Utf8 Path");

    let creator = super::Creator::new(
        super::NewOptions {
            project_root: path.to_string(),
            template: super::Template::Erlang,
            name: None,
            skip_git: true,
            skip_github: false,
        },
        "1.0.0-gleam",
    )
    .unwrap();
    creator.run().unwrap();
    assert!(!path.join(".git").exists());
    assert!(!path.join(".github").exists());
}

#[test]
fn new_with_skip_github() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.path().join("my_project")).expect("Non Utf8 Path");

    let creator = super::Creator::new(
        super::NewOptions {
            project_root: path.to_string(),
            template: super::Template::Erlang,
            name: None,
            skip_git: false,
            skip_github: true,
        },
        "1.0.0-gleam",
    )
    .unwrap();
    creator.run().unwrap();

    assert!(path.join(".git").exists());

    assert!(!path.join(".github").exists());
    assert!(!path.join(".github/workflows/test.yml").exists());
}

#[test]
fn new_with_skip_git_and_github() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.path().join("my_project")).expect("Non Utf8 Path");

    let creator = super::Creator::new(
        super::NewOptions {
            project_root: path.to_string(),
            template: super::Template::Erlang,
            name: None,
            skip_git: true,
            skip_github: true,
        },
        "1.0.0-gleam",
    )
    .unwrap();
    creator.run().unwrap();

    assert!(!path.join(".git").exists());

    assert!(!path.join(".github").exists());
    assert!(!path.join(".github/workflows/test.yml").exists());
}

#[test]
fn invalid_path() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.path().join("-------")).expect("Non Utf8 Path");

    assert!(
        super::Creator::new(
            super::NewOptions {
                project_root: path.to_string(),
                template: super::Template::Erlang,
                name: None,
                skip_git: false,
                skip_github: false,
            },
            "1.0.0-gleam",
        )
        .is_err()
    );
}

#[test]
fn invalid_name() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.path().join("projec")).expect("Non Utf8 Path");

    assert!(
        super::Creator::new(
            super::NewOptions {
                project_root: path.to_string(),
                template: super::Template::Erlang,
                name: Some("-".into()),
                skip_git: false,
                skip_github: false,
            },
            "1.0.0-gleam",
        )
        .is_err()
    );
}

#[test]
fn existing_directory_no_files() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.path().join("my_project")).expect("Non Utf8 Path");

    crate::fs::mkdir(&path).unwrap();

    let creator = super::Creator::new(
        super::NewOptions {
            project_root: path.to_string(),
            template: super::Template::Erlang,
            name: None,
            skip_git: true,
            skip_github: true,
        },
        "1.0.0-gleam",
    )
    .unwrap();

    creator.run().unwrap();

    assert!(path.join("README.md").exists());
}

#[test]
fn existing_directory_with_one_existing_file() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.path().join("my_project")).expect("Non Utf8 Path");

    crate::fs::mkdir(&path).unwrap();

    let _ = std::fs::File::create(PathBuf::from(&path).join("README.md")).unwrap();
    let _ = std::fs::File::create(PathBuf::from(&path).join("my_project.gleam")).unwrap();

    assert!(
        super::Creator::new(
            super::NewOptions {
                project_root: path.to_string(),
                template: super::Template::Erlang,
                name: None,
                skip_git: true,
                skip_github: true,
            },
            "1.0.0-gleam",
        )
        .is_err()
    );
}

#[test]
fn existing_directory_with_non_generated_file() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.path().join("my_project")).expect("Non Utf8 Path");

    crate::fs::mkdir(&path).unwrap();
    let file_path = PathBuf::from(&path).join("some_fake_thing_that_is_not_generated.md");

    let _ = std::fs::File::create(file_path).unwrap();

    let creator = super::Creator::new(
        super::NewOptions {
            project_root: path.to_string(),
            template: super::Template::Erlang,
            name: None,
            skip_git: true,
            skip_github: true,
        },
        "1.0.0-gleam",
    )
    .unwrap();

    creator.run().unwrap();

    assert!(path.join("README.md").exists());
    assert!(
        path.join("some_fake_thing_that_is_not_generated.md")
            .exists()
    );
}

#[test]
fn conflict_with_existing_files() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.path().join("my_project")).expect("Non Utf8 Path");

    crate::fs::mkdir(&path).unwrap();

    let _ = std::fs::File::create(PathBuf::from(&path).join("README.md")).unwrap();

    assert_eq!(
        super::Creator::new(
            super::NewOptions {
                project_root: path.to_string(),
                template: super::Template::Erlang,
                name: None,
                skip_git: true,
                skip_github: true,
            },
            "1.0.0-gleam",
        )
        .err(),
        Some(Error::OutputFilesAlreadyExist {
            file_names: vec![path.join("README.md")]
        })
    );
}

#[test]
fn skip_existing_git_files_when_skip_git_is_true() {
    let tmp = tempfile::tempdir().unwrap();
    let path = Utf8PathBuf::from_path_buf(tmp.path().join("my_project")).expect("Non Utf8 Path");

    crate::fs::mkdir(&path).unwrap();
    let file_path = PathBuf::from(&path).join(".gitignore");

    let _ = std::fs::File::create(file_path).unwrap();

    let creator = super::Creator::new(
        super::NewOptions {
            project_root: path.to_string(),
            template: super::Template::Erlang,
            name: None,
            skip_git: true,
            skip_github: true,
        },
        "1.0.0-gleam",
    )
    .unwrap();

    creator.run().unwrap();

    assert!(path.join("README.md").exists());
    assert!(path.join(".gitignore").exists());
}

#[test]
fn validate_name_format() {
    assert!(crate::new::validate_name("project").is_ok());
    assert!(crate::new::validate_name("project_name").is_ok());
    assert!(crate::new::validate_name("project2").is_ok());

    let invalid = ["Project", "PROJECT", "Project_Name"];
    for name in invalid {
        assert!(matches!(
            crate::new::validate_name(name),
            Err(Error::InvalidProjectName {
                name: _,
                reason: crate::new::InvalidProjectNameReason::FormatNotLowercase
            })
        ));
    }

    let invalid = ["0project", "_project", "project-name"];
    for name in invalid {
        assert!(matches!(
            crate::new::validate_name(name),
            Err(Error::InvalidProjectName {
                name: _,
                reason: crate::new::InvalidProjectNameReason::Format
            })
        ));
    }
}

#[test]
fn suggest_valid_names() {
    assert_eq!(
        crate::new::suggest_valid_name(
            "gleam_",
            &crate::new::InvalidProjectNameReason::GleamPrefix
        ),
        None
    );
    assert_eq!(
        crate::new::suggest_valid_name(
            "gleam_project",
            &crate::new::InvalidProjectNameReason::GleamPrefix
        ),
        Some("project".to_string())
    );

    assert_eq!(
        crate::new::suggest_valid_name(
            "try",
            &crate::new::InvalidProjectNameReason::ErlangReservedWord
        ),
        Some("try_app".to_string())
    );

    assert_eq!(
        crate::new::suggest_valid_name(
            "erl_eval",
            &crate::new::InvalidProjectNameReason::ErlangStandardLibraryModule
        ),
        Some("erl_eval_app".to_string())
    );

    assert_eq!(
        crate::new::suggest_valid_name(
            "assert",
            &crate::new::InvalidProjectNameReason::GleamReservedWord
        ),
        Some("assert_app".to_string())
    );

    assert_eq!(
        crate::new::suggest_valid_name(
            "gleam",
            &crate::new::InvalidProjectNameReason::GleamReservedModule
        ),
        Some("app_gleam".to_string())
    );

    assert_eq!(
        crate::new::suggest_valid_name(
            "Project_Name",
            &crate::new::InvalidProjectNameReason::FormatNotLowercase
        ),
        Some("project_name".to_string())
    );

    assert_eq!(
        crate::new::suggest_valid_name(
            "Pr0ject-n4me!",
            &crate::new::InvalidProjectNameReason::Format
        ),
        Some("pr0ject_n4me_".to_string())
    );

    assert_eq!(
        crate::new::suggest_valid_name(
            "Pr0ject--n4me!",
            &crate::new::InvalidProjectNameReason::Format
        ),
        Some("pr0ject_n4me_".to_string())
    );

    assert_eq!(
        crate::new::suggest_valid_name(
            "_pr0ject-name",
            &crate::new::InvalidProjectNameReason::Format
        ),
        None
    );
}
