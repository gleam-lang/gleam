use crate::{
    build::{project_root::ProjectRoot, Module},
    config::PackageConfig,
    erl,
    file::{self, OutputFile},
};
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

        for module in self.modules {
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

        // Render ebin/package.app
        outputs.push(self.package_app_file());

        outputs
    }

    pub fn package_app_file(&self) -> OutputFile {
        let path = self
            .root
            .default_build_lib_package_ebin_path(&self.config.name)
            .join(format!("{}.app", &self.config.name));

        // TODO: include a `mod` field if there is a start function to call
        let start_module = "";

        // TODO: applications, description, modules, registered, vsn
        let text = format!(
            r#"{{application, {package}, [
{start_module}    {{applications, [{applications}]}},
    {{description, "{description}"}},
    {{modules, [{modules}]}},
    {{registered, [{registered}]}},
    {{vsn, "{version}"}}
]}}.
"#,
            applications = "",
            description = self.config.description,
            modules = "",
            package = self.config.name,
            registered = "",
            start_module = start_module,
            version = self
                .config
                .version
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or(""), // TODO: enforce version
        );

        OutputFile { path, text }
    }
}
