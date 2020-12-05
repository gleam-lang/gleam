use crate::{build::Module, codegen::CodeGenerator, config::PackageConfig, erl, fs::OutputFile};
use std::path::PathBuf;

/// A code generator that creates a .erl Erlang module for each Gleam module in
/// the package.
#[derive(Debug)]
pub struct ErlangModules {
    output_directory: PathBuf,
}

impl CodeGenerator for ErlangModules {
    fn render(&self, _config: &PackageConfig, modules: &[Module]) -> Vec<OutputFile> {
        modules
            .iter()
            .map(|module| self.render_module(module))
            .collect()
    }
}

impl ErlangModules {
    pub fn new(output_directory: PathBuf) -> Self {
        Self { output_directory }
    }

    pub fn render_module(&self, module: &Module) -> OutputFile {
        let erl_name = module.name.replace("/", "@");
        let text = erl::module(&module.ast, &[]);
        let name = format!("{}.erl", erl_name);
        tracing::trace!(name = ?name, "Generated Erlang module");
        let path = self.output_directory.join(name);
        OutputFile { path, text }
    }
}
