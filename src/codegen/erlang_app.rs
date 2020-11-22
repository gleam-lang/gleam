use crate::{build::Module, codegen::CodeGenerator, config::PackageConfig, fs::OutputFile};
use itertools::Itertools;
use std::path::PathBuf;

/// A code generator that creates a .app Erlang application file for the package
#[derive(Debug)]
pub struct ErlangApp {
    output_directory: PathBuf,
}

impl CodeGenerator for ErlangApp {
    fn render(&self, config: &PackageConfig, modules: &[Module]) -> Vec<OutputFile> {
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

        vec![OutputFile { path, text }]
    }
}

impl ErlangApp {
    pub fn new(output_directory: PathBuf) -> Self {
        Self { output_directory }
    }
}

fn tuple(key: &str, value: &str) -> String {
    format!("    {{{}, {}}},\n", key, value)
}
