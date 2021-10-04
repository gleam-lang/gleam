use std::path::PathBuf;

pub fn package_cache_tarball(package_name: &str, version: &str) -> PathBuf {
    packages_cache().join(format!("{}-{}.tar", package_name, version))
}

fn packages_cache() -> PathBuf {
    default_gleam_cache()
        .join("hex")
        .join("hexpm")
        .join("packages")
}

pub fn default_gleam_cache() -> PathBuf {
    dirs::cache_dir()
        .expect("Failed to determine user cache directory")
        .join("gleam")
}

#[test]
fn paths() {
    assert!(default_gleam_cache().ends_with("gleam"));

    assert!(packages_cache().ends_with("hex/hexpm/packages"));

    assert!(package_cache_tarball("gleam_stdlib", "0.17.1")
        .ends_with("hex/hexpm/packages/gleam_stdlib-0.17.1.tar"));

    assert!(package_cache_tarball("elli", "1.0.0").ends_with("hex/hexpm/packages/elli-1.0.0.tar"));
}
