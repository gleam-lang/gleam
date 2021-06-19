//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using the Cap'n Proto schema.

mod module_decoder;
mod module_encoder;

#[cfg(test)]
mod tests;

pub use self::{module_decoder::ModuleDecoder, module_encoder::ModuleEncoder};
