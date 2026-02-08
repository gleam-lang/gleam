//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using serde.

#[cfg(test)]
mod tests;

use crate::{type_::ModuleInterface, uid::UniqueIdGenerator};

pub fn encode(module: &ModuleInterface) -> Result<Vec<u8>, bincode::error::EncodeError> {
    bincode::serde::encode_to_vec(module, bincode::config::legacy())
}

pub fn decode(
    bytes: &[u8],
    _ids: UniqueIdGenerator,
) -> Result<ModuleInterface, bincode::error::DecodeError> {
    bincode::serde::decode_from_slice(bytes, bincode::config::legacy()).map(|(module, _)| module)
}
