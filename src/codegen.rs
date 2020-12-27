mod erlang_app;

use crate::{build::Module, erl, fs::FileWriter, Result};
use std::{fmt::Debug, path::Path};

/// A code generator that creates a .erl Erlang module for each Gleam module in
/// the package.
#[derive(Debug)]
pub struct Erlang<'a> {
    output_directory: &'a Path,
}

impl<'a> Erlang<'a> {
    pub fn new(output_directory: &'a Path) -> Self {
        Self { output_directory }
    }

    pub fn render(&self, writer: &impl FileWriter, modules: &[Module]) -> Result<()> {
        for module in modules {
            let erl_name = module.name.replace("/", "@");
            self.erlang_module(writer, module, erl_name.as_str())?;
            self.erlang_record_headers(writer, module, erl_name.as_str())?;
        }
        Ok(())
    }

    fn erlang_module(
        &self,
        writer: &impl FileWriter,
        module: &Module,
        erl_name: &str,
    ) -> Result<()> {
        let text = erl::module(&module.ast);
        let name = format!("{}.erl", erl_name);
        tracing::trace!(name = ?name, "Generated Erlang module");
        writer
            .open(self.output_directory.join(name).as_path())?
            .write(text.as_bytes())?;

        Ok(())
    }

    fn erlang_record_headers(
        &self,
        writer: &dyn FileWriter,
        module: &Module,
        erl_name: &str,
    ) -> Result<()> {
        for (name, text) in erl::records(&module.ast).into_iter() {
            let name = format!("{}_{}.hrl", erl_name, name);
            tracing::trace!(name = ?name, "Generated Erlang header");
            writer
                .open(self.output_directory.join(name).as_path())?
                .write(text.as_bytes())?;
        }
        Ok(())
    }
}
