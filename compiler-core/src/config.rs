use crate::{Error, Result};
use hexpm::version::Version;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::PathBuf;

use crate::build::Mode;

pub fn default_version() -> Version {
    Version::parse("0.1.0").expect("default version")
}

pub type Dependencies = HashMap<String, hexpm::version::Range>;

#[derive(Deserialize, Debug, PartialEq)]
pub struct PackageConfig {
    pub name: String,
    #[serde(default = "default_version")]
    pub version: Version,
    #[serde(default)]
    pub description: String,
    #[serde(default)]
    pub docs: Docs,
    #[serde(default)]
    pub dependencies: Dependencies,
    #[serde(default, rename = "dev-dependencies")]
    pub dev_dependencies: Dependencies,
    #[serde(default)]
    pub otp_start_module: Option<String>,
    #[serde(default)]
    pub repository: Repository,
}

impl PackageConfig {
    pub fn dependencies_for(&self, mode: Mode) -> Result<Dependencies> {
        match mode {
            Mode::Dev => self.all_dependencies(),
            Mode::Prod => Ok(self.dependencies.clone()),
        }
    }

    pub fn all_dependencies(&self) -> Result<Dependencies> {
        let mut deps =
            HashMap::with_capacity(self.dependencies.len() + self.dev_dependencies.len());
        for (name, requirement) in self.dependencies.iter().chain(self.dev_dependencies.iter()) {
            let already_inserted = deps.insert(name.clone(), requirement.clone()).is_some();
            if already_inserted {
                // TODO: error to say dep has been duplicated
                return Err(Error::DuplicateDependency(name.clone()));
            }
        }
        Ok(deps)
    }
}

impl Default for PackageConfig {
    fn default() -> Self {
        Self {
            name: Default::default(),
            version: default_version(),
            description: Default::default(),
            docs: Default::default(),
            dependencies: Default::default(),
            otp_start_module: Default::default(),
            repository: Default::default(),
            dev_dependencies: Default::default(),
        }
    }
}

#[derive(Deserialize, Debug, PartialEq)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Repository {
    GitHub { user: String, repo: String },
    GitLab { user: String, repo: String },
    BitBucket { user: String, repo: String },
    Custom { url: String },
    None,
}

impl Repository {
    pub fn url(&self) -> Option<String> {
        match self {
            Repository::GitHub { repo, user } => {
                Some(format!("https://github.com/{}/{}", user, repo))
            }
            Repository::GitLab { repo, user } => {
                Some(format!("https://gitlab.com/{}/{}", user, repo))
            }
            Repository::BitBucket { repo, user } => {
                Some(format!("https://bitbucket.com/{}/{}", user, repo))
            }
            Repository::Custom { url } => Some(url.clone()),
            Repository::None => None,
        }
    }
}

impl Default for Repository {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Deserialize, Default, Debug, PartialEq)]
pub struct Docs {
    #[serde(default)]
    pub pages: Vec<DocsPage>,
    #[serde(default)]
    pub links: Vec<DocsLink>,
}

#[derive(Deserialize, Debug, PartialEq, Clone)]
pub struct DocsPage {
    pub title: String,
    pub path: String,
    pub source: PathBuf,
}

#[derive(Deserialize, Debug, PartialEq, Clone)]
pub struct DocsLink {
    pub title: String,
    pub href: String,
}
