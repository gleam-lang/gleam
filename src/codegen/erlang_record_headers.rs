use crate::{build::Module, erl, fs::FileWriter, CodeGenerator, Error};
use std::path::PathBuf;

/// A code generator that creates a .hrl Erlang header file containing a record
/// definition for each Gleam custom type with all named fields in the package.
#[derive(Debug)]
pub struct ErlangRecordHeaders {
    output_directory: PathBuf,
}

impl CodeGenerator for ErlangRecordHeaders {
    fn render(&self, writer: &dyn FileWriter, modules: &[Module]) -> Result<(), Error> {
        for module in modules {
            let erl_name = module.name.replace("/", "@");

            for (name, text) in erl::records(&module.ast).into_iter() {
                let name = format!("{}_{}.hrl", erl_name, name);
                tracing::trace!(name = ?name, "Generated Erlang header");
                writer
                    .open(self.output_directory.join(name).as_path())?
                    .write(text.as_bytes())?;
            }
        }
        Ok(())
    }
}

impl ErlangRecordHeaders {
    pub fn new(output_directory: PathBuf) -> Self {
        Self { output_directory }
    }
}
