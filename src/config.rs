use crate::error::{Error, FileIOAction, FileKind};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Deserialize, Debug, PartialEq, Default)]
pub struct PackageConfig {
    pub name: String,
    pub version: String,
    #[serde(default)]
    pub description: String,
    #[serde(default)]
    pub tool: BuildTool,
    #[serde(default)]
    pub docs: Docs,
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
    #[serde(default)]
    pub otp_start_module: Option<String>,
    #[serde(default)]
    pub repository: Repository,
}

#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub enum BuildTool {
    Gleam,
    Other,
}

impl Default for BuildTool {
    fn default() -> Self {
        Self::Other
    }
}

#[derive(Deserialize, Debug, PartialEq)]
#[serde(tag = "type")]
pub enum Repository {
    GitHub { user: String, repo: String },
    GitLab { user: String, repo: String },
    BitBucket { user: String, repo: String },
    Custom { url: String },
    None,
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

pub fn read_project_config(root: impl AsRef<Path>) -> Result<PackageConfig, Error> {
    let config_path = root.as_ref().join("gleam.toml");
    let toml = crate::fs::read(&config_path)?;
    toml::from_str(&toml).map_err(|e| Error::FileIO {
        action: FileIOAction::Parse,
        kind: FileKind::File,
        path: config_path.clone(),
        err: Some(e.to_string()),
    })
}
