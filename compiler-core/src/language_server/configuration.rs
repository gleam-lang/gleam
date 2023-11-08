use serde::Deserialize;

#[derive(Debug, Clone, Default)]
pub struct VersionedConfig {
    version: u32,
    config: Configuration,
}
impl VersionedConfig {
    pub fn update(&mut self, config: Configuration) {
        self.version += 1;
        self.config = config;
    }
}
impl PartialEq for VersionedConfig {
    fn eq(&self, other: &Self) -> bool {
        self.version == other.version
    }
}
impl std::ops::Deref for VersionedConfig {
    type Target = Configuration;

    fn deref(&self) -> &Self::Target {
        &self.config
    }
}

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
}

impl Default for InlayHintsConfig {
    fn default() -> Self {
        Self {
            function_definitions: Self::default_function_definitions(),
            module_constants: Self::default_module_constants(),
        }
    }
}

impl InlayHintsConfig {
    fn default_function_definitions() -> bool {
        false
    }
    fn default_module_constants() -> bool {
        false
    }
}
