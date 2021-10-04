use std::path::{Path, PathBuf};

pub fn package_cache_tarball(cache_path: &Path, package_name: &str, version: &str) -> PathBuf {
    packages_cache(cache_path).join(format!("{}-{}.tar", package_name, version))
}

fn packages_cache(cache_path: &Path) -> PathBuf {
    cache_path.join("hex").join("hexpm").join("packages")
}

pub fn default_gleam_cache() -> PathBuf {
    dirs::cache_dir()
        .expect("Failed to determine user cache directory")
        .join("gleam")
}

#[test]
fn paths() {
    assert!(default_gleam_cache().ends_with("gleam"));

    assert_eq!(
        packages_cache(&PathBuf::from("/some/where")),
        PathBuf::from("/some/where/hex/hexpm/packages")
    );

    assert_eq!(
        package_cache_tarball(&PathBuf::from("/some/where"), "gleam_stdlib", "0.17.1"),
        PathBuf::from("/some/where/hex/hexpm/packages/gleam_stdlib-0.17.1.tar")
    );

    assert_eq!(
        package_cache_tarball(&PathBuf::from("/some/where"), "elli", "1.0.0"),
        PathBuf::from("/some/where/hex/hexpm/packages/elli-1.0.0.tar")
    );
}
