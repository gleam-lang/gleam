use crate::{build::Module, config::PackageConfig, erl, fs::FileWriter, CodeGenerator, Error};
use std::path::PathBuf;

/// A code generator that creates a .erl Erlang module for each Gleam module in
/// the package.
#[derive(Debug)]
pub struct ErlangModules {
    output_directory: PathBuf,
}

impl CodeGenerator for ErlangModules {
    fn render(
        &self,
        writer: &dyn FileWriter,
        _config: &PackageConfig,
        modules: &[Module],
    ) -> Result<(), Error> {
        for module in modules {
            let erl_name = module.name.replace("/", "@");
            let text = erl::module(&module.ast);
            let name = format!("{}.erl", erl_name);
            tracing::trace!(name = ?name, "Generated Erlang module");
            writer
                .open(self.output_directory.join(name).as_path())?
                .write(text.as_bytes())?;
        }
        Ok(())
    }
}

impl ErlangModules {
    pub fn new(output_directory: PathBuf) -> Self {
        Self { output_directory }
    }
}
