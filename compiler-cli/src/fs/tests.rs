use std::path::Path;

use tempdir::TempDir;

#[test]
fn is_inside_git_work_tree_ok() {
    let tmp_dir = TempDir::new("gleam_git_check").unwrap();
    let path = tmp_dir.path();

    assert!(!super::is_inside_git_work_tree(path).unwrap());
    assert_eq!(super::git_init(path), Ok(()));
    assert!(super::is_inside_git_work_tree(path).unwrap())
}

#[test]
fn git_init_success() {
    let tmp_dir = TempDir::new("gleam_git_init").unwrap();
    let path = tmp_dir.path();
    let git = path.join(".git");

    assert!(!git.exists());
    assert_eq!(super::git_init(path), Ok(()));
    assert!(git.exists());
}

#[test]
fn git_init_already_in_git() {
    let tmp_dir = TempDir::new("gleam_git_init").unwrap();
    let git = tmp_dir.path().join(".git");
    assert!(!git.exists());
    assert_eq!(super::git_init(tmp_dir.path()), Ok(()));
    assert!(git.exists());

    let sub = tmp_dir.path().join("subproject");
    let git = sub.join(".git");
    crate::fs::mkdir(&sub).unwrap();
    assert!(!git.exists());
    assert_eq!(super::git_init(&sub), Ok(()));
    assert!(!git.exists());
}

#[test]
fn is_gleam_path_test() {
    assert!(super::is_gleam_path(
        Path::new("/some-prefix/a.gleam"),
        Path::new("/some-prefix/")
    ));

    assert!(super::is_gleam_path(
        Path::new("/some-prefix/one_two/a.gleam"),
        Path::new("/some-prefix/")
    ));

    assert!(super::is_gleam_path(
        Path::new("/some-prefix/one_two/a123.gleam"),
        Path::new("/some-prefix/")
    ));

    assert!(super::is_gleam_path(
        Path::new("/some-prefix/one_2/a123.gleam"),
        Path::new("/some-prefix/")
    ));
}
