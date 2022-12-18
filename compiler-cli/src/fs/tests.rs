use tempdir::TempDir;

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
