mod erlang_modules;

pub use erlang_modules::ErlangModules;

use crate::{build::Module, config::PackageConfig, fs::OutputFile};

pub trait CodeGenerator {
    fn render(&self, config: &PackageConfig, modules: &[Module]) -> Vec<OutputFile>;
}
