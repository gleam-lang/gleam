mod erlang_app;
mod erlang_modules;
mod erlang_record_headers;

pub use erlang_modules::ErlangModules;
pub use erlang_record_headers::ErlangRecordHeaders;

use crate::{build::Module, fs::FileWriter, Error};
use std::fmt::Debug;

pub trait CodeGenerator: Debug {
    fn render(&self, writer: &dyn FileWriter, modules: &[Module]) -> Result<(), Error>;
}
