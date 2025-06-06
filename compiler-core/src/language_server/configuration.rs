use serde::Deserialize;

#[derive(Debug, Default, Clone, Deserialize, PartialEq, Eq)]
#[serde(default)]
#[serde(rename_all = "camelCase")]
pub struct Configuration {
    pub inlay_hints: InlayHintsConfig,
}

#[derive(Debug, Default, Clone, Deserialize, PartialEq, Eq)]
#[serde(default)]
#[serde(rename_all = "camelCase")]
pub struct InlayHintsConfig {
    /// Whether to show type inlay hints of multiline pipelines
    pub pipelines: bool,

    /// Whether to show type inlay hints of function parameters
    pub function_parameter_types: bool,

    /// Whether to show type inlay hints of return types of functions
    pub function_return_types: bool,
}
