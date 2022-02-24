use crate::{
    build::Module,
    config::PackageConfig,
    erlang,
    io::{FileSystemWriter, Utf8Writer},
    javascript,
    line_numbers::LineNumbers,
    Result,
};
use itertools::Itertools;
use std::{fmt::Debug, path::Path};

/// A code generator that creates a .erl Erlang module and record header files
/// for each Gleam module in the package.
#[derive(Debug)]
pub struct Erlang<'a> {
    build_directory: &'a Path,
    include_directory: &'a Path,
}

impl<'a> Erlang<'a> {
    pub fn new(build_directory: &'a Path, include_directory: &'a Path) -> Self {
        Self {
            build_directory,
            include_directory,
        }
    }

    pub fn render<Writer: FileSystemWriter>(
        &self,
        writer: Writer,
        modules: &[Module],
    ) -> Result<()> {
        for module in modules {
            let erl_name = module.name.replace('/', "@");
            self.erlang_module(&writer, module, &erl_name)?;
            self.erlang_record_headers(&writer, module, &erl_name)?;
        }
        Ok(())
    }

    fn erlang_module<Writer: FileSystemWriter>(
        &self,
        writer: &Writer,
        module: &Module,
        erl_name: &str,
    ) -> Result<()> {
        let name = format!("{}.erl", erl_name);
        let path = self.build_directory.join(&name);
        let mut file = writer.writer(&path)?;
        let line_numbers = LineNumbers::new(&module.code);
        let res = erlang::module(&module.ast, &line_numbers, &mut file);
        tracing::debug!(name = ?name, "Generated Erlang module");
        res
    }

    fn erlang_record_headers<Writer: FileSystemWriter>(
        &self,
        writer: &Writer,
        module: &Module,
        erl_name: &str,
    ) -> Result<()> {
        for (name, text) in erlang::records(&module.ast) {
            let name = format!("{}_{}.hrl", erl_name, name);
            tracing::debug!(name = ?name, "Generated Erlang header");
            writer
                .writer(&self.include_directory.join(name))?
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

    pub fn render<Writer: FileSystemWriter>(
        &self,
        writer: Writer,
        config: &PackageConfig,
        modules: &[Module],
    ) -> Result<()> {
        fn tuple(key: &str, value: &str) -> String {
            format!("    {{{}, {}}},\n", key, value)
        }

        let path = self.output_directory.join(format!("{}.app", &config.name));

        let start_module = config
            .erlang
            .application_start_module
            .as_ref()
            .map(|module| tuple("mod", &format!("'{}'", module.replace('/', "@"))))
            .unwrap_or_default();

        let modules = modules
            .iter()
            .map(|m| m.name.replace('/', "@"))
            .sorted()
            .join(",\n               ");

        // TODO: When precompiling for production (i.e. as a precompiled hex
        // package) we will need to exclude the dev deps.
        let applications = config
            .dependencies
            .keys()
            .chain(config.dev_dependencies.keys())
            .chain(config.erlang.extra_applications.iter())
            .sorted()
            .join(",\n                    ");

        let text = format!(
            r#"{{application, {package}, [
{start_module}    {{vsn, "{version}"}},
    {{applications, [{applications}]}},
    {{description, "{description}"}},
    {{modules, [{modules}]}},
    {{registered, []}}
]}}.
"#,
            applications = applications,
            description = config.description,
            modules = modules,
            package = config.name,
            start_module = start_module,
            version = config.version,
        );

        writer.writer(&path)?.write(text.as_bytes())
    }
}

#[derive(Debug)]
pub struct JavaScript<'a> {
    output_directory: &'a Path,
}

impl<'a> JavaScript<'a> {
    pub fn new(output_directory: &'a Path) -> Self {
        Self { output_directory }
    }

    pub fn render(&self, writer: &impl FileSystemWriter, modules: &[Module]) -> Result<()> {
        for module in modules {
            let js_name = module.name.clone();
            self.js_module(writer, module, &js_name)?
        }
        self.write_prelude(writer)?;
        Ok(())
    }

    fn write_prelude(&self, writer: &impl FileSystemWriter) -> Result<()> {
        tracing::debug!("Generated js prelude");
        writer
            .writer(&self.output_directory.join("gleam.mjs"))?
            .str_write(javascript::PRELUDE)?;
        Ok(())
    }

    fn js_module(
        &self,
        writer: &impl FileSystemWriter,
        module: &Module,
        js_name: &str,
    ) -> Result<()> {
        let name = format!("{}.mjs", js_name);
        let path = self.output_directory.join(&name);
        let mut file = writer.writer(&path)?;
        let line_numbers = LineNumbers::new(&module.code);
        let res = javascript::module(
            &module.ast,
            &line_numbers,
            &module.input_path,
            &module.code,
            &mut file,
        );
        tracing::debug!(name = ?js_name, "Generated js module");
        res
    }
}
