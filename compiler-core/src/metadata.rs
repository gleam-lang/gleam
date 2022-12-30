//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using the Serde and Bincode

#[cfg(test)]
mod tests;

use std::collections::HashMap;
use crate::type_::{fn_, generic_var, tuple, Type, TypeVar};
use crate::uid::UniqueIdGenerator;
use crate::{type_::Module, Result};
use std::sync::Arc;

pub fn encode(data: &Module) -> Result<Vec<u8>> {
    let span = tracing::info_span!("metadata");
    let _enter = span.enter();

    let buffer = bincode::serialize(data).expect("failed to encode metadata (bincode)");

    Ok(buffer)
}

fn undo_links(id_generator: &UniqueIdGenerator, id_map: &mut HashMap<u64, u64>, type_var: TypeVar) -> Arc<Type> {
    match type_var {
        TypeVar::Link { type_ } => regenerate_type_ids(id_generator, id_map, &type_),
        TypeVar::Generic { id: old_id } => generic_var(match id_map.get(&old_id) {
            Some(&id) => id,
            None => {
                let new_id = id_generator.next();
                _ = id_map.insert(old_id, new_id);
                new_id
            }
        }),
        TypeVar::Unbound { .. } => panic!("unexpected `TypeVar::Unbound` in cache decoding"),
    }
}

fn regenerate_type_ids(id_generator: &UniqueIdGenerator, id_map: &mut HashMap<u64, u64>, type_: &Type) -> Arc<Type> {
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
                .map(|arg| regenerate_type_ids(&id_generator, id_map, &arg))
                .collect(),
        }),
        Type::Fn { args, retrn } => fn_(
            args.into_iter()
                .map(|arg| regenerate_type_ids(&id_generator, id_map, &arg))
                .collect(),
            regenerate_type_ids(&id_generator, id_map, &retrn),
        ),
        Type::Var { type_ } => undo_links(id_generator, id_map, (*type_).clone().into_inner()),
        Type::Tuple { elems } => tuple(
            elems
                .into_iter()
                .map(|elm| regenerate_type_ids(&id_generator, id_map, &elm))
                .collect(),
        ),
    }
}

pub fn decode(id_generator: UniqueIdGenerator, slice: &[u8]) -> Result<Module> {
    let mut module: Module =
        bincode::deserialize(slice).expect("failed to decode metadata (bincode)");

    let mut id_map: HashMap<u64, u64> = HashMap::new();

    module.types.iter_mut().for_each(|(_key, type_)| {
        type_.typ = regenerate_type_ids(&id_generator, &mut id_map, &type_.typ);
    });

    Ok(module)
}
