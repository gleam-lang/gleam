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
fn exclude_build_dir() {
    /*
    a
    |-- gleam.toml
    |-- build
    |  |-- f.gleam  # do not count as gleam file
    b
    |-- build
    |  |-- f.gleam  # count as gleam file
     */

    let tmp_dir = tempfile::tempdir().unwrap();
    let path = Utf8Path::from_path(tmp_dir.path()).expect("Non Utf-8 Path");

    // excluded gleam file
    {
        let gleam_toml = path.join("a/gleam.toml").to_path_buf();
        super::write(&gleam_toml, "").unwrap();

        let gleam_file = path.join("a/build/f.gleam").to_path_buf();
        super::write(&gleam_file, "").unwrap();
    };

    // included gleam file
    let gleam_file = path.join("b/build/f.gleam").to_path_buf();
    super::write(&gleam_file, "").unwrap();

    let files = super::gleam_files_excluding_gitignore(path).collect::<Vec<_>>();

    assert_eq!(files, vec![gleam_file]);
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

#[test]
fn extract_distro_id_test() {
    let os_release = "
PRETTY_NAME=\"Debian GNU/Linux 12 (bookworm)\"
NAME=\"Debian GNU/Linux\"
VERSION_ID=\"12\"
VERSION=\"12 (bookworm)\"
VERSION_CODENAME=bookworm
ID=debian
HOME_URL=\"https://www.debian.org/\"
";
    assert_eq!(super::extract_distro_id(os_release.to_string()), "debian");

    let os_release = "
VERSION_CODENAME=jammy
ID=ubuntu
ID_LIKE=debian
HOME_URL=\"https://www.ubuntu.com/\"
";
    assert_eq!(super::extract_distro_id(os_release.to_string()), "ubuntu");

    assert_eq!(super::extract_distro_id("".to_string()), "");
    assert_eq!(super::extract_distro_id("\n".to_string()), "");
    assert_eq!(super::extract_distro_id("ID=".to_string()), "");
    assert_eq!(super::extract_distro_id("ID= ".to_string()), " ");
    assert_eq!(
        super::extract_distro_id("ID= space test ".to_string()),
        " space test "
    );
    assert_eq!(super::extract_distro_id("id=ubuntu".to_string()), "");
    assert_eq!(
        super::extract_distro_id("NAME=\"Debian\"\nID=debian".to_string()),
        "debian"
    );
    assert_eq!(
        super::extract_distro_id("\n\nNAME=\n\n\nID=test123\n".to_string()),
        "test123"
    );
    assert_eq!(
        super::extract_distro_id("\nID=\"id first\"\nID=another_id".to_string()),
        "id first"
    );
}
