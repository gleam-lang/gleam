use crate::{
    build::{project_root::ProjectRoot, Module},
    config::PackageConfig,
    erl,
    file::{self, OutputFile},
};
use itertools::Itertools;
use std::collections::HashMap;

#[derive(Debug)]
pub struct ErlangCodeGenerator<'a> {
    root: &'a ProjectRoot,
    config: &'a PackageConfig,
    modules: &'a [Module],
}

// TODO: test: A couple of packages with a couple of modules and headers.
// Make sure all file names are right etc.
impl<'a> ErlangCodeGenerator<'a> {
    pub fn new(root: &'a ProjectRoot, config: &'a PackageConfig, modules: &'a [Module]) -> Self {
        Self {
            root,
            config,
            modules,
        }
    }

    pub fn render(&self) -> Vec<OutputFile> {
        let num_modules = self.modules.len();
        let mut outputs = Vec::with_capacity(num_modules);

        // Render Erlang modules and header files
        for module in self.modules {
            self.render_module(module, &mut outputs);
        }

        // Render ebin/package.app
        outputs.push(self.package_app_file());

        outputs
    }

    pub fn render_module(&self, module: &Module, outputs: &mut Vec<OutputFile>) {
        let erl_name = module.name.replace("/", "@");
        let dir = self
            .root
            .default_build_lib_package_source_path(&self.config.name, module.origin);

        // Render record header files
        for (name, text) in erl::records(&module.ast).into_iter() {
            let name = format!("{}_{}.hrl", erl_name, name);
            tracing::trace!(name = ?name, "Generated Erlang header");
            outputs.push(OutputFile {
                path: dir.join(name),
                text,
            });
        }

        // Render Erlang module file
        let text = erl::module(&module.ast);
        let name = format!("{}.erl", erl_name);
        tracing::trace!(name = ?name, "Generated Erlang module");
        let path = dir.join(name);
        outputs.push(OutputFile { path, text });
    }

    pub fn package_app_file(&self) -> OutputFile {
        let path = self
            .root
            .default_build_lib_package_ebin_path(&self.config.name)
            .join(format!("{}.app", &self.config.name));

        let start_module = match &self.config.otp_start_module {
            None => "".to_string(),
            Some(module) => tuple("mod", format!("'{}'", module).as_str()),
        };

        let version = match &self.config.version {
            None => "".to_string(),
            Some(version) => tuple("vsn", format!("\"{}\"", version).as_str()),
        };

        let mut modules: Vec<_> = self
            .modules
            .iter()
            .map(|m| m.name.replace("/", "@"))
            .collect();
        modules.sort();
        let modules = modules.join(", ");

        // TODO: applications
        let text = format!(
            r#"{{application, {package}, [
{start_module}{version}    {{applications, [{applications}]}},
    {{description, "{description}"}},
    {{modules, [{modules}]}},
    {{registered, []}},
]}}.
"#,
            applications = "",
            description = self.config.description,
            modules = modules,
            package = self.config.name,
            start_module = start_module,
            version = version,
        );

        OutputFile { path, text }
    }
}

fn tuple(key: &str, value: &str) -> String {
    format!("    {{{}, {}}}\n", key, value)
}
