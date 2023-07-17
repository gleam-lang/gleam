use camino::Utf8Path;

#[test]
fn is_inside_git_work_tree_ok() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let path = Utf8Path::from_path(tmp_dir.path()).expect("Non Utf-8 Path");

    assert!(!super::is_inside_git_work_tree(path).unwrap());
    assert_eq!(super::git_init(path), Ok(()));
    assert!(super::is_inside_git_work_tree(path).unwrap())
}

#[test]
fn git_init_success() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let path = Utf8Path::from_path(tmp_dir.path()).expect("Non Utf-8 Path");
    let git = path.join(".git");

    assert!(!git.exists());
    assert_eq!(super::git_init(path), Ok(()));
    assert!(git.exists());
}

#[test]
fn git_init_already_in_git() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let git = Utf8Path::from_path(tmp_dir.path())
        .expect("Non Utf-8 Path")
        .join(".git");
    assert!(!git.exists());
    assert_eq!(
        super::git_init(Utf8Path::from_path(tmp_dir.path()).expect("Non Utf-8 Path")),
        Ok(())
    );
    assert!(git.exists());

    let sub = Utf8Path::from_path(tmp_dir.path())
        .expect("Non Utf-8 Path")
        .join("subproject");
    let git = sub.join(".git");
    crate::fs::mkdir(&sub).unwrap();
    assert!(!git.exists());
    assert_eq!(super::git_init(&sub), Ok(()));
    assert!(!git.exists());
}

#[test]
fn is_gleam_path_test() {
    assert!(super::is_gleam_path(
        Utf8Path::new("/some-prefix/a.gleam"),
        Utf8Path::new("/some-prefix/")
    ));

    assert!(super::is_gleam_path(
        Utf8Path::new("/some-prefix/one_two/a.gleam"),
        Utf8Path::new("/some-prefix/")
    ));

    assert!(super::is_gleam_path(
        Utf8Path::new("/some-prefix/one_two/a123.gleam"),
        Utf8Path::new("/some-prefix/")
    ));

    assert!(super::is_gleam_path(
        Utf8Path::new("/some-prefix/one_2/a123.gleam"),
        Utf8Path::new("/some-prefix/")
    ));
}
