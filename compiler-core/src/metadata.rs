//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using the Serde and Bincode

#[cfg(test)]
mod tests;

use crate::type_::{Type, TypeVar};
use crate::uid::UniqueIdGenerator;
use crate::{
    type_::{Module, TypeConstructor},
    Result,
};
use std::cell::RefCell;
use std::sync::Arc;

#[derive(Debug, Clone, Copy)]
pub struct Metadata;

impl Metadata {
    pub fn encode(data: &Module) -> Result<Vec<u8>> {
        let span = tracing::info_span!("metadata");
        let _enter = span.enter();

        let config = bincode::config::standard();
        let buffer = bincode::serde::encode_to_vec(data, config).expect("bincode");

        Ok(buffer)
    }

    fn undo_links(id_generator: &UniqueIdGenerator, type_var: TypeVar) -> Type {
        match type_var {
            TypeVar::Link { type_ } => Metadata::decode_type(id_generator, &type_),
            TypeVar::Generic { .. } => Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Generic {
                    id: id_generator.next(),
                })),
            },
            TypeVar::Unbound { .. } => Type::Var {
                type_: Arc::new(RefCell::new(TypeVar::Unbound {
                    id: id_generator.next(),
                })),
            },
        }
    }

    fn decode_type(id_generator: &UniqueIdGenerator, type_: &Type) -> Type {
        match (*type_).clone() {
            Type::App {
                public,
                module,
                name,
                args,
            } => Type::App {
                public: public.clone(),
                module: module.clone(),
                name: name.clone(),
                args: args
                    .into_iter()
                    .map(|arg| Arc::new(Metadata::decode_type(&id_generator, &arg)))
                    .collect::<Vec<Arc<Type>>>(),
            },
            Type::Fn { args, retrn } => Type::Fn {
                args: args
                    .into_iter()
                    .map(|arg| Arc::new(Metadata::decode_type(&id_generator, &arg)))
                    .collect::<Vec<Arc<Type>>>(),
                retrn: Arc::new(Metadata::decode_type(&id_generator, &retrn)),
            },
            Type::Var { type_ } => {
                Metadata::undo_links(id_generator, (*type_).clone().into_inner())
            }
            Type::Tuple { elems } => Type::Tuple {
                elems: elems
                    .into_iter()
                    .map(|elm| Arc::new(Metadata::decode_type(&id_generator, &elm)))
                    .collect::<Vec<Arc<Type>>>(),
            },
        }
    }

    pub fn decode(
        id_generator: UniqueIdGenerator,
        slice: &[u8],
    ) -> Result<Module> {
        let config = bincode::config::standard();
        let (module, _) : (Module, usize)= bincode::serde::decode_from_slice(slice, config).expect("bincode");

        Ok(Module {
            types: module
                .types
                .into_iter()
                .enumerate()
                .map(|(_, (key, type_))| -> (String, TypeConstructor) {
                    (
                        key,
                        TypeConstructor {
                            typ: Arc::new(Metadata::decode_type(&id_generator, &type_.typ)),
                            ..type_
                        },
                    )
                })
                .collect(),
            ..module
        })
    }
}
