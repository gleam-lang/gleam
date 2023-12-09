//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using the Cap'n Proto schema.

mod module_decoder;
mod module_encoder;

#[cfg(test)]
mod tests;

use capnp::text::Reader;
use ecow::EcoString;

pub use self::{module_decoder::ModuleDecoder, module_encoder::ModuleEncoder};

trait ReaderExt {
    fn as_ecostring(&self) -> EcoString;
    fn as_str(&self) -> &str;
}

impl ReaderExt for Reader<'_> {
    fn as_ecostring(&self) -> EcoString {
        self.as_str().into()
    }

    fn as_str(&self) -> &str {
        self.to_str().expect("Invalid UTF-8 string")
    }
}
