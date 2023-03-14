use std::path::{Path, PathBuf};

use crate::build::{Mode, Target};

pub const ARTEFACT_DIRECTORY_NAME: &str = "_gleam_artefacts";

#[derive(Debug, Clone)]
pub struct ProjectPaths {
    root: PathBuf,
}

impl ProjectPaths {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    pub fn at_filesystem_root() -> Self {
        Self::new(PathBuf::from("/"))
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn root_config(&self) -> PathBuf {
        self.root.join("gleam.toml")
    }

    pub fn readme(&self) -> PathBuf {
        self.root.join("README.md")
    }

    pub fn manifest(&self) -> PathBuf {
        self.root.join("manifest.toml")
    }

    pub fn src_directory(&self) -> PathBuf {
        self.root.join("src")
    }

    pub fn build_directory(&self) -> PathBuf {
        self.root.join("build")
    }

    pub fn build_packages_directory(&self) -> PathBuf {
        self.build_directory().join("packages")
    }

    pub fn build_packages_toml(&self) -> PathBuf {
        self.build_packages_directory().join("packages.toml")
    }

    pub fn build_packages_package(&self, package_name: &str) -> PathBuf {
        self.build_packages_directory().join(package_name)
    }

    // build_deps_package_config
    pub fn build_packages_package_config(&self, package_name: &str) -> PathBuf {
        self.build_packages_package(package_name).join("gleam.toml")
    }

    pub fn build_export_hex_tarball(&self, package_name: &str, version: &str) -> PathBuf {
        self.build_directory()
            .join(format!("{package_name}-{version}.tar"))
    }

    pub fn build_directory_for_mode(&self, mode: Mode) -> PathBuf {
        self.build_directory().join(mode.to_string())
    }

    pub fn erlang_shipment_directory(&self) -> PathBuf {
        self.build_directory().join("erlang-shipment")
    }

    pub fn build_documentation_directory(&self, package: &str) -> PathBuf {
        self.build_directory_for_mode(Mode::Dev)
            .join("docs")
            .join(package)
    }

    pub fn build_directory_for_target(&self, mode: Mode, target: Target) -> PathBuf {
        self.build_directory_for_mode(mode).join(target.to_string())
    }

    pub fn build_directory_for_package(
        &self,
        mode: Mode,
        target: Target,
        package: &str,
    ) -> PathBuf {
        self.build_directory_for_target(mode, target).join(package)
    }

    pub fn build_packages_ebins_glob(&self, mode: Mode, target: Target) -> PathBuf {
        self.build_directory_for_package(mode, target, "*")
            .join("ebin")
    }

    /// A path to a special file that contains the version of gleam that last built
    /// the artifacts. If this file does not match the current version of gleam we
    /// will rebuild from scratch
    pub fn build_gleam_version(&self, mode: Mode, target: Target) -> PathBuf {
        self.build_directory_for_target(mode, target)
            .join("gleam_version")
    }
}

pub fn global_package_cache_package_tarball(package_name: &str, version: &str) -> PathBuf {
    global_packages_cache().join(format!("{package_name}-{version}.tar"))
}

fn global_packages_cache() -> PathBuf {
    default_global_gleam_cache()
        .join("hex")
        .join("hexpm")
        .join("packages")
}

pub fn default_global_gleam_cache() -> PathBuf {
    dirs_next::cache_dir()
        .expect("Failed to determine user cache directory")
        .join("gleam")
}

pub fn unnest(within: &Path) -> PathBuf {
    let mut path = PathBuf::new();
    for _ in within {
        path = path.join("..")
    }
    path
}

#[test]
fn paths() {
    assert!(default_global_gleam_cache().ends_with("gleam"));

    assert!(global_packages_cache().ends_with("hex/hexpm/packages"));

    assert!(
        global_package_cache_package_tarball("gleam_stdlib", "0.17.1")
            .ends_with("hex/hexpm/packages/gleam_stdlib-0.17.1.tar")
    );

    assert!(global_package_cache_package_tarball("elli", "1.0.0")
        .ends_with("hex/hexpm/packages/elli-1.0.0.tar"));
}
