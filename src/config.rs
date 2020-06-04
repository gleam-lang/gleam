use crate::{
    error::{Error, FileIOAction, FileKind},
    file,
};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Deserialize, Debug, PartialEq)]
pub struct PackageConfig {
    pub name: String,
    #[serde(default)]
    pub version: Option<String>,
    #[serde(default)]
    pub description: String,
    #[serde(default = "BuildTool::default")]
    pub tool: BuildTool,
    #[serde(default)]
    pub docs: Docs,
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
}

#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub enum BuildTool {
    Gleam,
    Other,
}

impl BuildTool {
    pub fn default() -> Self {
        Self::Other
    }
}

#[derive(Deserialize, Default, Debug, PartialEq)]
pub struct Docs {
    pub pages: Vec<DocsPage>,
}

#[derive(Deserialize, Debug, PartialEq, Clone)]
pub struct DocsPage {
    pub title: String,
    pub path: String,
    pub source: PathBuf,
}

pub fn read_project_config(root: impl AsRef<Path>) -> Result<PackageConfig, Error> {
    let config_path = root.as_ref().join("gleam.toml");
    let toml = file::read(&config_path)?;
    toml::from_str(&toml).map_err(|e| Error::FileIO {
        action: FileIOAction::Parse,
        kind: FileKind::File,
        path: config_path.clone(),
        err: Some(e.to_string()),
    })
}
