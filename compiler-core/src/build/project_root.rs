use crate::{
    build::Origin,
    config::{self, PackageConfig},
    error::Error,
};
use std::collections::HashMap;
use std::path::PathBuf;

// Directory names
const DIR_NAME_BUILD: &str = "_build";
const DIR_NAME_PROFILE_DEFAULT: &str = "default";
const DIR_NAME_LIB: &str = "lib";
const DIR_NAME_PACKAGE_SRC: &str = "src";
const DIR_NAME_PACKAGE_TEST: &str = "test";
const DIR_NAME_PACKAGE_EBIN: &str = "ebin";

#[derive(Debug)]
pub struct ProjectRoot;

impl ProjectRoot {
    pub fn new() -> Self {
        Self
    }

    pub fn src_path(&self) -> PathBuf {
        PathBuf::from("./").join(DIR_NAME_PACKAGE_SRC)
    }

    pub fn build_path(&self) -> PathBuf {
        PathBuf::from("./").join(DIR_NAME_BUILD)
    }

    pub fn default_build_lib_path(&self) -> PathBuf {
        self.build_path()
            .join(DIR_NAME_PROFILE_DEFAULT)
            .join(DIR_NAME_LIB)
    }

    pub fn default_build_lib_package_path(&self, name: &str) -> PathBuf {
        self.default_build_lib_path().join(name)
    }

    pub fn default_build_lib_package_source_path(&self, name: &str, origin: Origin) -> PathBuf {
        match origin {
            Origin::Src => self.default_build_lib_package_src_path(name),
            Origin::Test => self.default_build_lib_package_test_path(name),
        }
    }

    pub fn default_build_lib_package_src_path(&self, name: &str) -> PathBuf {
        self.default_build_lib_package_path(name)
            .join(DIR_NAME_PACKAGE_SRC)
    }

    pub fn default_build_lib_package_test_path(&self, name: &str) -> PathBuf {
        self.default_build_lib_package_path(name)
            .join(DIR_NAME_PACKAGE_TEST)
    }

    pub fn default_build_lib_package_ebin_path(&self, name: &str) -> PathBuf {
        self.default_build_lib_package_path(name)
            .join(DIR_NAME_PACKAGE_EBIN)
    }
}
