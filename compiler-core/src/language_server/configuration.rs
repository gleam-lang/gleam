use serde::Deserialize;
use std::sync::{Arc, RwLock};

pub type SharedConfig = Arc<RwLock<Configuration>>;

#[derive(Debug, Default, Clone, Deserialize)]
pub struct Configuration {
    #[serde(default = "InlayHintsConfig::default")]
    pub inlay_hints: InlayHintsConfig,
}

#[derive(Debug, Clone, Deserialize)]
pub struct InlayHintsConfig {
    /// Whether to show type inlay hints of multiline pipelines
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
