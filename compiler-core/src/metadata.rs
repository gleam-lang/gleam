//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using the Serde and Bincode

#[cfg(test)]
mod tests;

use itertools::Itertools;

use crate::{
    ast::{
        BitStringSegment, BitStringSegmentOption, CallArg, Constant, SrcSpan, TypedConstant,
        TypedConstantBitStringSegment, TypedConstantBitStringSegmentOption,
    },
    build::Origin,
    compiler_cache,
    type_::{
        self, AccessorsMap, FieldMap, Module, RecordAccessor, Type, TypeConstructor,
        ValueConstructor, ValueConstructorVariant,
    },
    uid::UniqueIdGenerator,
    Result,
};
use std::{collections::HashMap, io::BufRead, sync::Arc};

pub struct Metadata;

impl Metadata {
    pub fn encode(data: Module) -> Result<Vec<u8>> {
        let span = tracing::info_span!("metadata");
        let _enter = span.enter();

        let types = data
            .types
            .iter()
            .enumerate()
            .map(|(_, (key, value))| {
                (
                    key,
                    compiler_cache::TypeConstructor {
                        _type: value.typ.clone(),
                        parameters: value.parameters.clone(),
                        module: value.module.clone(),
                    },
                )
            })
            .collect::<HashMap<String, compiler_cache::TypeConstructor>>();

        let values = data
            .values
            .iter()
            .enumerate()
            .map(|(_, (key, value))| {
                (
                    key,
                    compiler_cache::ValueConstructor {
                        _type: value.type_.clone(),
                        variant: value.variant.clone(),
                    },
                )
            })
            .collect::<HashMap<String, compiler_cache::ValueConstructor>>();

        let accessors = data
            .accessors
            .iter()
            .enumerate()
            .map(|(_, (key, value))| {
                (
                    key,
                    compiler_cache::AccessorsHashMap {
                        _type: value.type_.clone(),
                        accessors: value
                            .accessors
                            .iter()
                            .enumerate()
                            .map(|(_, (key, value))| {
                                (
                                    key,
                                    compiler_cache::RecordAccessor {
                                        _type: value.type_.clone(),
                                        index: value.index.clone(),
                                        label: value.label.clone(),
                                    },
                                )
                            })
                            .collect::<HashMap<String, compiler_cache::RecordAccessor>>(),
                    },
                )
            })
            .collect::<HashMap<String, compiler_cache::AccessorsHashMap>>();

        let types_constructors = data
            .types_constructors
            .iter()
            .enumerate()
            .map(|(_, (key, value))| (key, *(value.clone())))
            .collect::<HashMap<String, Vec<String>>>();

        let mut module = compiler_cache::Module {
            name: data.name.iter().collect::<Vec<String>>(),
            types,
            values,
            accessors,
            types_constructors,
            package: data.package.clone(),
        };

        let config = bincode::config::standard();
        let buffer = bincode::encode_to_vec(module, config).expect("bincode");

        Ok(buffer)
    }

    pub fn decode(reader: impl BufRead) -> Result<Module> {
        let config = bincode::config::standard();
        let module: compiler_cache::Module =
            bincode::decode_from_reader(reader, config).expect("bincode");

        Ok(Module {
            name: module.name,
            package: module.package,
            origin: Origin::Src,
            types: module
                .types
                .iter()
                .enumerate()
                .map(|(_, (key, value))| {
                    (
                        key,
                        TypeConstructor {
                            public: false,
                            origin: Default::default(),
                            module: value.module.clone(),
                            parameters: value.parameters.clone(),
                            typ: value._type.clone(),
                        },
                    )
                })
                .collect::<HashMap<String, TypeConstructor>>(),
            types_constructors: module.types_constructors,
            values: module
                .values
                .iter()
                .enumerate()
                .map(|(_, (key, value))| {
                    (
                        key,
                        ValueConstructor {
                            public: false,
                            variant: value.variant.clone(),
                            type_: value._type.clone(),
                        },
                    )
                })
                .collect::<HashMap<String, ValueConstructor>>(),
            accessors: module
                .accessors
                .iter()
                .enumerate()
                .map(|(_, (key, value))| {
                    (
                        key,
                        AccessorsMap {
                            public: false,
                            type_: value._type.clone(),
                            accessors: value
                                .accessors
                                .iter()
                                .enumerate()
                                .map(|(_, (key, value))| {
                                    (
                                        key,
                                        RecordAccessor {
                                            index: value.index,
                                            label: value.label.clone(),
                                            type_: value._type.clone(),
                                        },
                                    )
                                })
                                .collect::<HashMap<String, RecordAccessor>>(),
                        },
                    )
                })
                .collect::<HashMap<String, AccessorsMap>>(),
        })
    }
}
