//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using the Serde and Bincode

#[cfg(test)]
mod tests;

use crate::type_::{generic_var, Type, TypeVar};
use crate::uid::UniqueIdGenerator;
use crate::{
    type_::Module,
    Result,
};
use std::sync::Arc;

pub fn encode(data: &Module) -> Result<Vec<u8>> {
    let span = tracing::info_span!("metadata");
    let _enter = span.enter();

    let buffer = bincode::serialize(data).expect("failed to encode metadata (bincode)");

    Ok(buffer)
}

fn undo_links(id_generator: &UniqueIdGenerator, type_var: TypeVar) -> Arc<Type> {
    match type_var {
        TypeVar::Link { type_ } => decode_type(id_generator, &type_),
        TypeVar::Generic { .. } => generic_var(id_generator.next()),
        TypeVar::Unbound { .. } => panic!("unexpected `TypeVar::Unbound` in cache decoding"),
    }
}

fn decode_type(id_generator: &UniqueIdGenerator, type_: &Type) -> Arc<Type> {
    match (*type_).clone() {
        Type::App {
            public,
            module,
            name,
            args,
        } => Arc::new(Type::App {
            public,
            module,
            name,
            args: args
                .into_iter()
                .map(|arg| decode_type(&id_generator, &arg))
                .collect(),
        }),
        Type::Fn { args, retrn } => Arc::new(Type::Fn {
            args: args
                .into_iter()
                .map(|arg| decode_type(&id_generator, &arg))
                .collect(),
            retrn: decode_type(&id_generator, &retrn),
        }),
        Type::Var { type_ } => undo_links(id_generator, (*type_).clone().into_inner()),
        Type::Tuple { elems } => Arc::new(Type::Tuple {
            elems: elems
                .into_iter()
                .map(|elm| decode_type(&id_generator, &elm))
                .collect(),
        }),
    }
}

pub fn decode(id_generator: UniqueIdGenerator, slice: &[u8]) -> Result<Module> {
    let mut module: Module = bincode::deserialize(slice).expect("failed to decode metadata (bincode)");

    Ok(Module {
    })
}
