#[test]
fn new() {
    let tmp = tempfile::tempdir().unwrap();
    let path = tmp.path().join("my_project");

    let creator = super::Creator::new(
        super::NewOptions {
            project_root: path.to_str().unwrap().to_string(),
            template: super::Template::Lib,
            name: None,
            description: "Wibble wobble".into(),
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
    assert!(toml.contains("description = \"Wibble wobble\""));
}

#[test]
fn invalid_path() {
    let tmp = tempfile::tempdir().unwrap();
    let path = tmp.path().join("-------");

    assert!(super::Creator::new(
        super::NewOptions {
            project_root: path.to_str().unwrap().to_string(),
            template: super::Template::Lib,
            name: None,
            description: "Wibble wobble".into(),
        },
        "1.0.0-gleam",
    )
    .is_err());
}

#[test]
fn invalid_name() {
    let tmp = tempfile::tempdir().unwrap();
    let path = tmp.path().join("projec");

    assert!(super::Creator::new(
        super::NewOptions {
            project_root: path.to_str().unwrap().to_string(),
            template: super::Template::Lib,
            name: Some("-".into()),
            description: "Wibble wobble".into(),
        },
        "1.0.0-gleam",
    )
    .is_err());
}
