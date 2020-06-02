use crate::{
    config::{self, PackageConfig},
    error::Error,
    file,
};
use std::collections::HashMap;
use std::path::PathBuf;

// Directory names
const DIR_NAME_BUILD: &str = "_build";
const DIR_NAME_PROFILE_DEFAULT: &str = "default";
const DIR_NAME_LIB: &str = "lib";
const DIR_NAME_PACKAGE_SRC: &str = "src";
const DIR_NAME_PACKAGE_EBIN: &str = "ebin";

#[derive(Debug)]
pub struct ProjectRoot {
    path: PathBuf,
}

impl ProjectRoot {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }

    pub fn package_configs(&self) -> Result<HashMap<String, PackageConfig>, Error> {
        file::read_dir(self.default_build_lib_path())?
            .filter_map(Result::ok)
            .map(|dir_entry| {
                let config = config::read_project_config(dir_entry.path())?;
                Ok((config.name.clone(), config))
            })
            .collect::<Result<_, _>>()
    }

    pub fn default_build_lib_path(&self) -> PathBuf {
        self.path
            .join(DIR_NAME_BUILD)
            .join(DIR_NAME_PROFILE_DEFAULT)
            .join(DIR_NAME_LIB)
    }

    pub fn default_build_lib_package_path(&self, name: &str) -> PathBuf {
        self.default_build_lib_path().join(name)
    }

    pub fn default_build_lib_package_src_path(&self, name: &str) -> PathBuf {
        self.default_build_lib_package_path(name)
            .join(DIR_NAME_PACKAGE_SRC)
    }
}
