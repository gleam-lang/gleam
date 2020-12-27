use crate::{build::Module, config::PackageConfig, erl, fs::FileWriter, Result};
use itertools::Itertools;
use std::{fmt::Debug, path::Path};

/// A code generator that creates a .erl Erlang module and record header files
/// for each Gleam module in the package.
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

/// A code generator that creates a .app Erlang application file for the package
#[derive(Debug)]
pub struct ErlangApp<'a> {
    output_directory: &'a Path,
}

impl<'a> ErlangApp<'a> {
    pub fn new(output_directory: &'a Path) -> Self {
        Self { output_directory }
    }

    pub fn render(
        &self,
        writer: &impl FileWriter,
        config: &PackageConfig,
        modules: &[Module],
    ) -> Result<()> {
        fn tuple(key: &str, value: &str) -> String {
            format!("    {{{}, {}}},\n", key, value)
        }

        let path = self.output_directory.join(format!("{}.app", &config.name));

        let start_module = match &config.otp_start_module {
            None => "".to_string(),
            Some(module) => tuple("mod", format!("'{}'", module).as_str()),
        };

        let modules = modules
            .iter()
            .map(|m| m.name.replace("/", "@"))
            .sorted()
            .join(",\n               ");

        let mut applications: Vec<_> = config.dependencies.iter().map(|m| m.0).collect();
        applications.sort();
        let applications = applications.into_iter().join(",\n                    ");

        let text = format!(
            r#"{{application, {package}, [
{start_module}    {{vsn, "{version}"}},
    {{applications, [{applications}]}},
    {{description, "{description}"}},
    {{modules, [{modules}]}},
    {{registered, []}},
]}}.
"#,
            applications = applications,
            description = config.description,
            modules = modules,
            package = config.name,
            start_module = start_module,
            version = config.version,
        );

        writer.open(&path)?.write(text.as_bytes())
    }
}
