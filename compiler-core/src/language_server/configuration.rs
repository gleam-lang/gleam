use serde::Deserialize;
use std::sync::{Arc, RwLock};

pub type SharedConfig = Arc<RwLock<Configuration>>;

#[derive(Debug, Default, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Configuration {
    #[serde(default = "InlayHintsConfig::default")]
    pub inlay_hints: InlayHintsConfig,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InlayHintsConfig {
    #[serde(default = "InlayHintsConfig::default_pipelines")]
    pub pipelines: bool,
}

impl Default for InlayHintsConfig {
    fn default() -> Self {
        Self {
            pipelines: Self::default_pipelines(),
        }
    }
}

impl InlayHintsConfig {
    fn default_pipelines() -> bool {
        false
    }
}
