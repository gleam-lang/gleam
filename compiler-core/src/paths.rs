use std::path::{Path, PathBuf};

use crate::build::{Mode, Target};

pub fn root_config() -> PathBuf {
    PathBuf::from("gleam.toml")
}

pub fn root() -> PathBuf {
    PathBuf::from("./")
}

pub fn readme() -> PathBuf {
    PathBuf::from("README.md")
}
pub fn build() -> PathBuf {
    PathBuf::from("build")
}

pub fn manifest() -> PathBuf {
    PathBuf::from("manifest.toml")
}

pub fn src() -> PathBuf {
    PathBuf::from("src")
}

pub fn test() -> PathBuf {
    PathBuf::from("test")
}

pub fn package_cache_tarball(package_name: &str, version: &str) -> PathBuf {
    packages_cache().join(format!("{}-{}.tar", package_name, version))
}

pub fn build_deps_package_src(package: &str) -> PathBuf {
    build_deps_package(package).join("src")
}

pub fn build_deps_package_config(package: &str) -> PathBuf {
    build_deps_package(package).join("gleam.toml")
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

pub fn build_packages_ebins_glob(mode: Mode, target: Target) -> PathBuf {
    build_packages_erl_libs_glob(mode, target).join("ebin")
}

pub fn build_packages_erl_libs_glob(mode: Mode, target: Target) -> PathBuf {
    build_package(mode, target, "*")
}

pub fn unnest(within: &Path) -> PathBuf {
    let mut path = PathBuf::new();
    for _ in within {
        path = path.join("..")
    }
    path
}

pub fn build_docs(package: &str) -> PathBuf {
    build()
        .join(Mode::Dev.to_string())
        .join("docs")
        .join(package)
}

pub fn build_package(mode: Mode, target: Target, package: &str) -> PathBuf {
    build_packages(mode, target).join(package)
}

/// A path to a special file that contains the version of gleam that last built
/// the artifacts. If this file does not match the current version of gleam we
/// will rebuild from scratch
pub fn build_gleam_version(mode: Mode, target: Target) -> PathBuf {
    build_packages(mode, target).join("gleam_version")
}

/// A path to a special file that contains the build journal of gleam that last built
/// the artifacts.
pub fn build_journal(mode: Mode, target: Target) -> PathBuf {
    build_packages(mode, target).join("gleam_build_journal")
}

#[test]
fn paths() {
    assert!(default_gleam_cache().ends_with("gleam"));

    assert!(packages_cache().ends_with("hex/hexpm/packages"));

    assert!(package_cache_tarball("gleam_stdlib", "0.17.1")
        .ends_with("hex/hexpm/packages/gleam_stdlib-0.17.1.tar"));

    assert!(package_cache_tarball("elli", "1.0.0").ends_with("hex/hexpm/packages/elli-1.0.0.tar"));
}
