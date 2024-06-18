use serde::Deserialize;
use std::sync::{Arc, RwLock};

pub type SharedConfig = Arc<RwLock<Configuration>>;

#[derive(Debug, Default, Clone, Deserialize, PartialEq, Eq)]
#[serde(default)]
pub struct Configuration {
    pub inlay_hints: InlayHintsConfig,
}

#[derive(Debug, Default, Clone, Deserialize, PartialEq, Eq)]
#[serde(default)]
pub struct InlayHintsConfig {
    /// Whether to show type inlay hints of multiline pipelines
    pub pipelines: bool,
}
