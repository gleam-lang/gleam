use std::path::PathBuf;

use crate::build::{Mode, Target};

pub fn build() -> PathBuf {
    PathBuf::from("build")
}

pub fn manifest_path() -> PathBuf {
    PathBuf::from("manifest.toml")
}

pub fn package_cache_tarball(package_name: &str, version: &str) -> PathBuf {
    packages_cache().join(format!("{}-{}.tar", package_name, version))
}

pub fn build_deps_package_src(package: &str) -> PathBuf {
    build_deps_package(package).join("src")
}

pub fn build_deps_package_test(package: &str) -> PathBuf {
    build_deps_package(package).join("test")
}

pub fn packages() -> PathBuf {
    build().join("packages")
}

pub fn packages_toml() -> PathBuf {
    packages().join("packages.toml")
}

pub fn build_deps_package(package: &str) -> PathBuf {
    packages().join(package)
}

pub fn build_scripts() -> PathBuf {
    build().join("scripts")
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

pub fn build_packages(mode: Mode, target: Target) -> PathBuf {
    build().join(mode.to_string()).join(target.to_string())
}

pub fn build_package(mode: Mode, target: Target, package: &str) -> PathBuf {
    build_packages(mode, target).join(package)
}

#[test]
fn paths() {
    assert!(default_gleam_cache().ends_with("gleam"));

    assert!(packages_cache().ends_with("hex/hexpm/packages"));

    assert!(package_cache_tarball("gleam_stdlib", "0.17.1")
        .ends_with("hex/hexpm/packages/gleam_stdlib-0.17.1.tar"));

    assert!(package_cache_tarball("elli", "1.0.0").ends_with("hex/hexpm/packages/elli-1.0.0.tar"));
}
