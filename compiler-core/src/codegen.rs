use crate::{
    Result,
    build::{
        ErlangAppCodegenConfiguration, Module, module_erlang_name, package_compiler::StdlibPackage,
    },
    config::PackageConfig,
    erlang,
    io::FileSystemWriter,
    javascript::{self, ModuleConfig},
    line_numbers::LineNumbers,
};
use ecow::EcoString;
use erlang::escape_atom_string;
use itertools::Itertools;
use std::fmt::Debug;

use camino::Utf8Path;

/// A code generator that creates a .erl Erlang module and record header files
/// for each Gleam module in the package.
#[derive(Debug)]
pub struct Erlang<'a> {
    build_directory: &'a Utf8Path,
    include_directory: &'a Utf8Path,
}

impl<'a> Erlang<'a> {
    pub fn new(build_directory: &'a Utf8Path, include_directory: &'a Utf8Path) -> Self {
        Self {
            build_directory,
            include_directory,
        }
    }

    pub fn render<Writer: FileSystemWriter>(
        &self,
        writer: Writer,
        modules: &[Module],
        root: &Utf8Path,
    ) -> Result<()> {
        for module in modules {
            let erl_name = module.erlang_name();
            self.erlang_module(&writer, module, &erl_name, root)?;
            self.erlang_record_headers(&writer, module, &erl_name)?;
        }
        Ok(())
    }

    fn erlang_module<Writer: FileSystemWriter>(
        &self,
        writer: &Writer,
        module: &Module,
        erl_name: &str,
        root: &Utf8Path,
    ) -> Result<()> {
        let name = format!("{erl_name}.erl");
        let path = self.build_directory.join(&name);
        let line_numbers = LineNumbers::new(&module.code);
        let output = erlang::module(&module.ast, &line_numbers, root);
        tracing::debug!(name = ?name, "Generated Erlang module");
        writer.write(&path, &output?)
    }

    fn erlang_record_headers<Writer: FileSystemWriter>(
        &self,
        writer: &Writer,
        module: &Module,
        erl_name: &str,
    ) -> Result<()> {
        for (name, text) in erlang::records(&module.ast) {
            let name = format!("{erl_name}_{name}.hrl");
            tracing::debug!(name = ?name, "Generated Erlang header");
            writer.write(&self.include_directory.join(name), &text)?;
        }
        Ok(())
    }
}

/// A code generator that creates a .app Erlang application file for the package
#[derive(Debug)]
pub struct ErlangApp<'a> {
    output_directory: &'a Utf8Path,
    config: &'a ErlangAppCodegenConfiguration,
}

impl<'a> ErlangApp<'a> {
    pub fn new(output_directory: &'a Utf8Path, config: &'a ErlangAppCodegenConfiguration) -> Self {
        Self {
            output_directory,
            config,
        }
    }

    pub fn render<Writer: FileSystemWriter>(
        &self,
        writer: Writer,
        config: &PackageConfig,
        modules: &[Module],
        native_modules: Vec<EcoString>,
    ) -> Result<()> {
        fn tuple(key: &str, value: &str) -> String {
            format!("    {{{key}, {value}}},\n")
        }

        let path = self.output_directory.join(format!("{}.app", &config.name));

        let start_module = match config.erlang.application_start_module.as_ref() {
            None => "".into(),
            Some(module) => {
                let module = module_erlang_name(module);
                let argument = match config.erlang.application_start_argument.as_ref() {
                    Some(argument) => argument.as_str(),
                    None => "[]",
                };
                tuple("mod", &format!("{{'{module}', {argument}}}"))
            }
        };

        let modules = modules
            .iter()
            .map(|m| m.erlang_name())
            .chain(native_modules)
            .unique()
            .sorted()
            .map(escape_atom_string)
            .join(",\n               ");

        // TODO: When precompiling for production (i.e. as a precompiled hex
        // package) we will need to exclude the dev deps.
        let applications = config
            .dependencies
            .keys()
            .chain(
                config
                    .dev_dependencies
                    .keys()
                    .take_while(|_| self.config.include_dev_deps),
            )
            // TODO: test this!
            .map(|name| self.config.package_name_overrides.get(name).unwrap_or(name))
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

        writer.write(&path, &text)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeScriptDeclarations {
    None,
    Emit,
}

#[derive(Debug)]
pub struct JavaScript<'a> {
    output_directory: &'a Utf8Path,
    prelude_location: &'a Utf8Path,
    project_root: &'a Utf8Path,
    typescript: TypeScriptDeclarations,
}

impl<'a> JavaScript<'a> {
    pub fn new(
        output_directory: &'a Utf8Path,
        typescript: TypeScriptDeclarations,
        prelude_location: &'a Utf8Path,
        project_root: &'a Utf8Path,
    ) -> Self {
        Self {
            prelude_location,
            output_directory,
            project_root,
            typescript,
        }
    }

    pub fn render(
        &self,
        writer: &impl FileSystemWriter,
        modules: &[Module],
        stdlib_package: StdlibPackage,
    ) -> Result<()> {
        for module in modules {
            let js_name = module.name.clone();
            if self.typescript == TypeScriptDeclarations::Emit {
                self.ts_declaration(writer, module, &js_name)?;
            }
            self.js_module(writer, module, &js_name, stdlib_package)?
        }
        self.write_prelude(writer)?;
        Ok(())
    }

    fn write_prelude(&self, writer: &impl FileSystemWriter) -> Result<()> {
        let rexport = format!("export * from \"{}\";\n", self.prelude_location);
        let prelude_path = &self.output_directory.join("gleam.mjs");

        // This check skips unnecessary `gleam.mjs` writes which confuse
        // watchers and HMR build tools
        if !writer.exists(prelude_path) {
            writer.write(prelude_path, &rexport)?;
        }

        if self.typescript == TypeScriptDeclarations::Emit {
            let rexport = format!(
                "export * from \"{}\";\nexport type * from \"{}\";\n",
                self.prelude_location,
                self.prelude_location.as_str().replace(".mjs", ".d.mts")
            );
            let prelude_declaration_path = &self.output_directory.join("gleam.d.mts");

            // Type declaration may trigger badly configured watchers
            if !writer.exists(prelude_declaration_path) {
                writer.write(prelude_declaration_path, &rexport)?;
            }
        }

        Ok(())
    }

    fn ts_declaration(
        &self,
        writer: &impl FileSystemWriter,
        module: &Module,
        js_name: &str,
    ) -> Result<()> {
        let name = format!("{js_name}.d.mts");
        let path = self.output_directory.join(name);
        let output = javascript::ts_declaration(&module.ast);
        tracing::debug!(name = ?js_name, "Generated TS declaration");
        writer.write(&path, &output)
    }

    fn js_module(
        &self,
        writer: &impl FileSystemWriter,
        module: &Module,
        js_name: &str,
        stdlib_package: StdlibPackage,
    ) -> Result<()> {
        let name = format!("{js_name}.mjs");
        let path = self.output_directory.join(name);
        let line_numbers = LineNumbers::new(&module.code);
        let output = javascript::module(ModuleConfig {
            module: &module.ast,
            line_numbers: &line_numbers,
            path: &module.input_path,
            project_root: self.project_root,
            src: &module.code,
            typescript: self.typescript,
            stdlib_package,
        });
        tracing::debug!(name = ?js_name, "Generated js module");
        writer.write(&path, &output)
    }
}
