use crate::error::{Error, FileIOAction, FileKind};
use serde::Deserialize;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

#[derive(Deserialize, Debug)]
pub struct ProjectConfig {
    pub name: String,
    #[serde(default = "BuildTool::default")]
    pub tool: BuildTool,
    pub docs: Option<DocsPages>,
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

#[derive(Deserialize, Debug)]
pub struct DocsPages {
    pub pages: Vec<DocsPage>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct DocsPage {
    pub title: String,
    pub path: String,
    pub source: PathBuf,
}

pub fn read_project_config(root: impl AsRef<Path>) -> Result<ProjectConfig, Error> {
    let config_path = root.as_ref().join("gleam.toml");

    let mut file = File::open(&config_path).map_err(|e| Error::FileIO {
        action: FileIOAction::Open,
        kind: FileKind::File,
        path: config_path.clone(),
        err: Some(e.to_string()),
    })?;

    let mut toml = String::new();
    file.read_to_string(&mut toml).map_err(|e| Error::FileIO {
        action: FileIOAction::Read,
        kind: FileKind::File,
        path: config_path.clone(),
        err: Some(e.to_string()),
    })?;

    let project_config = toml::from_str(&toml).map_err(|e| Error::FileIO {
        action: FileIOAction::Parse,
        kind: FileKind::File,
        path: config_path.clone(),
        err: Some(e.to_string()),
    })?;

    Ok(project_config)
}
