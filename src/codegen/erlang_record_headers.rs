use crate::{build::Module, codegen::CodeGenerator, config::PackageConfig, erl, fs::OutputFile};
use std::path::PathBuf;

// TODO: test
/// A code generator that creates a .hrl Erlang header file containing a record
/// definition for each Gleam custom type with all named fields in the package.
pub struct ErlangRecordHeaders {
    output_directory: PathBuf,
}

impl CodeGenerator for ErlangRecordHeaders {
    fn render(&self, _config: &PackageConfig, modules: &[Module]) -> Vec<OutputFile> {
        modules
            .iter()
            .flat_map(|module| self.render_record_headers(module))
            .collect()
    }
}

impl ErlangRecordHeaders {
    // TODO
    #[allow(unused)]
    pub fn new(output_directory: PathBuf) -> Self {
        Self { output_directory }
    }

    pub fn render_record_headers(&self, module: &Module) -> Vec<OutputFile> {
        let erl_name = module.name.replace("/", "@");
        erl::records(&module.ast)
            .into_iter()
            .map(|(name, text)| {
                let name = format!("{}_{}.hrl", erl_name, name);
                tracing::trace!(name = ?name, "Generated Erlang header");
                OutputFile {
                    path: self.output_directory.join(name),
                    text,
                }
            })
            .collect()
    }
}
