//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using serde and bincode

#[cfg(test)]
mod tests;

use crate::type_::ModuleInterface;
use crate::uid::UniqueIdGenerator;
use crate::Result;

use bincode;

pub fn encode(data: &ModuleInterface) -> Result<Vec<u8>> {
    let span = tracing::info_span!("metadata");
    let _enter = span.enter();

    let buffer = bincode::serialize(data).expect("failed to encode metadata (bincode)");

    Ok(buffer)
}

pub fn decode(_id_generator: UniqueIdGenerator, slice: &[u8]) -> Result<ModuleInterface> {
    let module = bincode::deserialize(slice).expect("failed to decode metadata (bincode)");

    Ok(module)
}
