mod erlang_app;
mod erlang_modules;
mod erlang_record_headers;

pub use erlang_app::ErlangApp;
pub use erlang_modules::ErlangModules;
pub use erlang_record_headers::ErlangRecordHeaders;

use crate::{build::Module, config::PackageConfig, fs::OutputFile};
use std::fmt::Debug;

pub trait CodeGenerator: Debug {
    fn render(&self, config: &PackageConfig, modules: &[Module]) -> Vec<OutputFile>;
}
