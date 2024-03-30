use serde::Deserialize;
use std::sync::{Arc, RwLock};

pub type SharedConfig = Arc<RwLock<Configuration>>;

#[derive(Debug, Default, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Configuration {
    pub inlay_hints: InlayHintsConfig,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InlayHintsConfig {
    #[serde(default = "InlayHintsConfig::default_function_definitions")]
    pub function_definitions: bool,

    #[serde(default = "InlayHintsConfig::default_module_constants")]
    pub module_constants: bool,

    #[serde(default = "InlayHintsConfig::default_variable_assignments")]
    pub variable_assignments: bool,
}

impl Default for InlayHintsConfig {
    fn default() -> Self {
        Self {
            function_definitions: Self::default_function_definitions(),
            module_constants: Self::default_module_constants(),
            variable_assignments: Self::default_variable_assignments(),
        }
    }
}

impl InlayHintsConfig {
    fn default_function_definitions() -> bool {
        true
    }
    fn default_module_constants() -> bool {
        true
    }
    fn default_variable_assignments() -> bool {
        true
    }
}
