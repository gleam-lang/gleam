use crate::build::{Mode, Target};

use camino::{Utf8Path, Utf8PathBuf};

pub const ARTEFACT_DIRECTORY_NAME: &str = "_gleam_artefacts";

#[derive(Debug, Clone)]
pub struct ProjectPaths {
    root: Utf8PathBuf,
}

impl ProjectPaths {
    pub fn new(root: Utf8PathBuf) -> Self {
        Self { root }
    }

    pub fn at_filesystem_root() -> Self {
        let path = if cfg!(target_family = "windows") {
            r"C:\"
        } else {
            "/"
        };

        Self::new(Utf8PathBuf::from(path))
    }

    pub fn root(&self) -> &Utf8Path {
        &self.root
    }

    pub fn root_config(&self) -> Utf8PathBuf {
        self.root.join("gleam.toml")
    }

    pub fn readme(&self) -> Utf8PathBuf {
        self.root.join("README.md")
    }

    pub fn manifest(&self) -> Utf8PathBuf {
        self.root.join("manifest.toml")
    }

    pub fn src_directory(&self) -> Utf8PathBuf {
        self.root.join("src")
    }

    pub fn test_directory(&self) -> Utf8PathBuf {
        self.root.join("test")
    }

    pub fn build_directory(&self) -> Utf8PathBuf {
        self.root.join("build")
    }

    pub fn build_packages_directory(&self) -> Utf8PathBuf {
        self.build_directory().join("packages")
    }

    pub fn build_packages_toml(&self) -> Utf8PathBuf {
        self.build_packages_directory().join("packages.toml")
    }

    pub fn build_packages_package(&self, package_name: &str) -> Utf8PathBuf {
        self.build_packages_directory().join(package_name)
    }

    // build_deps_package_config
    pub fn build_packages_package_config(&self, package_name: &str) -> Utf8PathBuf {
        self.build_packages_package(package_name).join("gleam.toml")
    }

    pub fn build_export_hex_tarball(&self, package_name: &str, version: &str) -> Utf8PathBuf {
        self.build_directory()
            .join(format!("{package_name}-{version}.tar"))
    }

    pub fn build_directory_for_mode(&self, mode: Mode) -> Utf8PathBuf {
        self.build_directory().join(mode.to_string())
    }

    pub fn erlang_shipment_directory(&self) -> Utf8PathBuf {
        self.build_directory().join("erlang-shipment")
    }

    pub fn build_documentation_directory(&self, package: &str) -> Utf8PathBuf {
        self.build_directory_for_mode(Mode::Dev)
            .join("docs")
            .join(package)
    }

    pub fn build_directory_for_target(&self, mode: Mode, target: Target) -> Utf8PathBuf {
        self.build_directory_for_mode(mode).join(target.to_string())
    }

    pub fn build_directory_for_package(
        &self,
        mode: Mode,
        target: Target,
        package: &str,
    ) -> Utf8PathBuf {
        self.build_directory_for_target(mode, target).join(package)
    }

    pub fn build_packages_ebins_glob(&self, mode: Mode, target: Target) -> Utf8PathBuf {
        self.build_directory_for_package(mode, target, "*")
            .join("ebin")
    }

    /// A path to a special file that contains the version of gleam that last built
    /// the artifacts. If this file does not match the current version of gleam we
    /// will rebuild from scratch
    pub fn build_gleam_version(&self, mode: Mode, target: Target) -> Utf8PathBuf {
        self.build_directory_for_target(mode, target)
            .join("gleam_version")
    }
}

pub fn global_package_cache_package_tarball(package_name: &str, version: &str) -> Utf8PathBuf {
    global_packages_cache().join(format!("{package_name}-{version}.tar"))
}

fn global_packages_cache() -> Utf8PathBuf {
    default_global_gleam_cache()
        .join("hex")
        .join("hexpm")
        .join("packages")
}

pub fn default_global_gleam_cache() -> Utf8PathBuf {
    Utf8PathBuf::from_path_buf(
        dirs_next::cache_dir()
            .expect("Failed to determine user cache directory")
            .join("gleam"),
    )
    .expect("Non Utf8 Path")
}

pub fn unnest(within: &Utf8Path) -> Utf8PathBuf {
    let mut path = Utf8PathBuf::new();
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
