//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using serde.

#[cfg(test)]
mod tests;

use std::{collections::HashMap, sync::Arc};

use crate::{
    ast::{
        BitArrayOption, BitArraySegment, CallArg, Constant, RecordBeingUpdated, RecordUpdateArg,
        TypedConstant,
    },
    type_::{
        self, AccessorsMap, ModuleInterface, RecordAccessor, Type, TypeAliasConstructor,
        TypeConstructor, TypeValueConstructor, TypeValueConstructorField, TypeVar,
        TypeVariantConstructors, ValueConstructor, ValueConstructorVariant,
    },
    uid::UniqueIdGenerator,
};

pub fn encode(module: &ModuleInterface) -> Result<Vec<u8>, bincode::error::EncodeError> {
    bincode::serde::encode_to_vec(module, bincode::config::legacy())
}

pub fn decode(
    bytes: &[u8],
    ids: UniqueIdGenerator,
) -> Result<ModuleInterface, bincode::error::DecodeError> {
    bincode::serde::decode_from_slice(bytes, bincode::config::legacy())
        .map(|(module, _)| remap_type_variable_ids(module, ids))
}

fn remap_type_variable_ids(module: ModuleInterface, ids: UniqueIdGenerator) -> ModuleInterface {
    RemapIds::new(ids).module(module)
}

struct RemapIds {
    ids: UniqueIdGenerator,
    remapped_variable_ids: HashMap<u64, u64>,
}

impl RemapIds {
    fn new(ids: UniqueIdGenerator) -> Self {
        Self {
            ids,
            remapped_variable_ids: HashMap::new(),
        }
    }

    fn module(&mut self, module: ModuleInterface) -> ModuleInterface {
        let ModuleInterface {
            name,
            origin,
            package,
            types,
            types_value_constructors,
            values,
            accessors,
            line_numbers,
            src_path,
            is_internal,
            warnings,
            minimum_required_version,
            type_aliases,
            documentation,
            contains_echo,
            references,
            inline_functions,
        } = module;

        let types = types
            .into_iter()
            .map(|(name, type_)| (name, self.type_constructor(type_)))
            .collect();
        let types_value_constructors = types_value_constructors
            .into_iter()
            .map(|(name, type_)| (name, self.type_variant_constructors(type_)))
            .collect();
        let values = values
            .into_iter()
            .map(|(name, value)| (name, self.value_constructor(value)))
            .collect();
        let accessors = accessors
            .into_iter()
            .map(|(name, accessors)| (name, self.accessors_map(accessors)))
            .collect();
        let type_aliases = type_aliases
            .into_iter()
            .map(|(name, type_)| (name, self.type_alias(type_)))
            .collect();

        ModuleInterface {
            name,
            origin,
            package,
            types,
            types_value_constructors,
            values,
            accessors,
            line_numbers,
            src_path,
            is_internal,
            warnings,
            minimum_required_version,
            type_aliases,
            documentation,
            contains_echo,
            references,
            inline_functions,
        }
    }

    fn type_constructor(&mut self, type_: TypeConstructor) -> TypeConstructor {
        let TypeConstructor {
            publicity,
            origin,
            module,
            parameters,
            type_,
            deprecation,
            documentation,
        } = type_;

        let parameters = parameters
            .into_iter()
            .map(|parameter| self.type_(parameter))
            .collect();
        let type_ = self.type_(type_);

        TypeConstructor {
            publicity,
            origin,
            module,
            parameters,
            type_,
            deprecation,
            documentation,
        }
    }

    fn type_variant_constructors(
        &mut self,
        type_: TypeVariantConstructors,
    ) -> TypeVariantConstructors {
        let TypeVariantConstructors {
            type_parameters_ids,
            opaque,
            variants,
        } = type_;

        let type_parameters_ids = type_parameters_ids
            .into_iter()
            .map(|id| self.id(id))
            .collect();

        let variants = variants
            .into_iter()
            .map(|variant| self.type_value_constructor(variant))
            .collect();

        TypeVariantConstructors {
            type_parameters_ids,
            opaque,
            variants,
        }
    }

    fn type_value_constructor(&mut self, variant: TypeValueConstructor) -> TypeValueConstructor {
        let TypeValueConstructor {
            name,
            parameters,
            documentation,
        } = variant;

        let parameters = parameters
            .into_iter()
            .map(
                |TypeValueConstructorField {
                     type_,
                     label,
                     documentation,
                 }| TypeValueConstructorField {
                    type_: self.type_(type_),
                    label,
                    documentation,
                },
            )
            .collect();

        TypeValueConstructor {
            name,
            parameters,
            documentation,
        }
    }

    fn value_constructor(&mut self, value: ValueConstructor) -> ValueConstructor {
        let ValueConstructor {
            publicity,
            deprecation,
            variant,
            type_,
        } = value;

        let variant = self.value_constructor_variant(variant);

        let type_ = self.type_(type_);

        ValueConstructor {
            publicity,
            deprecation,
            variant,
            type_,
        }
    }

    fn value_constructor_variant(
        &mut self,
        variant: ValueConstructorVariant,
    ) -> ValueConstructorVariant {
        match variant {
            ValueConstructorVariant::LocalVariable { location, origin } => {
                ValueConstructorVariant::LocalVariable { location, origin }
            }
            ValueConstructorVariant::ModuleConstant {
                documentation,
                location,
                module,
                name,
                literal,
                implementations,
            } => ValueConstructorVariant::ModuleConstant {
                documentation,
                location,
                module,
                name,
                literal: self.constant(literal),
                implementations,
            },
            ValueConstructorVariant::ModuleFn {
                name,
                field_map,
                module,
                arity,
                location,
                documentation,
                implementations,
                external_erlang,
                external_javascript,
                purity,
            } => ValueConstructorVariant::ModuleFn {
                name,
                field_map,
                module,
                arity,
                location,
                documentation,
                implementations,
                external_erlang,
                external_javascript,
                purity,
            },
            ValueConstructorVariant::Record {
                name,
                arity,
                field_map,
                location,
                module,
                variants_count,
                variant_index,
                documentation,
            } => ValueConstructorVariant::Record {
                name,
                arity,
                field_map,
                location,
                module,
                variants_count,
                variant_index,
                documentation,
            },
        }
    }

    fn constant(&mut self, constant: TypedConstant) -> TypedConstant {
        match constant {
            Constant::Int {
                location,
                value,
                int_value,
            } => Constant::Int {
                location,
                value,
                int_value,
            },
            Constant::Float {
                location,
                value,
                float_value,
            } => Constant::Float {
                location,
                value,
                float_value,
            },
            Constant::String { location, value } => Constant::String { location, value },
            Constant::Tuple {
                location,
                elements,
                type_,
            } => Constant::Tuple {
                location,
                elements: elements
                    .into_iter()
                    .map(|element| self.constant(element))
                    .collect(),
                type_: self.type_(type_),
            },
            Constant::List {
                location,
                elements,
                type_,
            } => Constant::List {
                location,
                elements: elements
                    .into_iter()
                    .map(|element| self.constant(element))
                    .collect(),
                type_: self.type_(type_),
            },
            Constant::Record {
                location,
                module,
                name,
                arguments,
                tag,
                type_,
                field_map,
                record_constructor,
            } => Constant::Record {
                location,
                module,
                name,
                arguments: arguments
                    .into_iter()
                    .map(
                        |CallArg {
                             label,
                             location,
                             value,
                             implicit,
                         }| CallArg {
                            label,
                            location,
                            value: self.constant(value),
                            implicit,
                        },
                    )
                    .collect(),
                tag,
                type_: self.type_(type_),
                field_map,
                record_constructor: record_constructor
                    .map(|constructor| Box::new(self.value_constructor(*constructor))),
            },
            Constant::RecordUpdate {
                location,
                constructor_location,
                module,
                name,
                record:
                    RecordBeingUpdated {
                        base: record,
                        location: record_location,
                    },
                arguments,
                tag,
                type_,
                field_map,
            } => Constant::RecordUpdate {
                location,
                constructor_location,
                module,
                name,
                record: RecordBeingUpdated {
                    base: Box::new(self.constant(*record)),
                    location: record_location,
                },
                arguments: arguments
                    .into_iter()
                    .map(
                        |RecordUpdateArg {
                             label,
                             location,
                             value,
                         }| RecordUpdateArg {
                            label,
                            location,
                            value: self.constant(value),
                        },
                    )
                    .collect(),
                tag,
                type_: self.type_(type_),
                field_map,
            },
            Constant::BitArray { location, segments } => Constant::BitArray {
                location,
                segments: segments
                    .into_iter()
                    .map(|segment| self.bit_array_segment(segment))
                    .collect(),
            },
            Constant::Var {
                location,
                module,
                name,
                constructor,
                type_,
            } => Constant::Var {
                location,
                module,
                name,
                constructor: constructor
                    .map(|constructor| Box::new(self.value_constructor(*constructor))),
                type_: self.type_(type_),
            },
            Constant::StringConcatenation {
                location,
                left,
                right,
            } => Constant::StringConcatenation {
                location,
                left: Box::new(self.constant(*left)),
                right: Box::new(self.constant(*right)),
            },
            Constant::Invalid {
                location,
                type_,
                extra_information,
            } => Constant::Invalid {
                location,
                type_: self.type_(type_),
                extra_information,
            },
        }
    }

    fn bit_array_segment(
        &mut self,
        segment: BitArraySegment<TypedConstant, Arc<Type>>,
    ) -> BitArraySegment<TypedConstant, Arc<Type>> {
        let BitArraySegment {
            location,
            value,
            options,
            type_,
        } = segment;

        let value = Box::new(self.constant(*value));
        let options = options
            .into_iter()
            .map(|option| self.bit_array_option(option))
            .collect();
        let type_ = self.type_(type_);

        BitArraySegment {
            location,
            value,
            options,
            type_,
        }
    }

    fn bit_array_option(
        &mut self,
        option: BitArrayOption<TypedConstant>,
    ) -> BitArrayOption<TypedConstant> {
        match option {
            BitArrayOption::Bytes { .. }
            | BitArrayOption::Int { .. }
            | BitArrayOption::Float { .. }
            | BitArrayOption::Bits { .. }
            | BitArrayOption::Utf8 { .. }
            | BitArrayOption::Utf16 { .. }
            | BitArrayOption::Utf32 { .. }
            | BitArrayOption::Utf8Codepoint { .. }
            | BitArrayOption::Utf16Codepoint { .. }
            | BitArrayOption::Utf32Codepoint { .. }
            | BitArrayOption::Signed { .. }
            | BitArrayOption::Unsigned { .. }
            | BitArrayOption::Big { .. }
            | BitArrayOption::Little { .. }
            | BitArrayOption::Native { .. }
            | BitArrayOption::Unit { .. } => option,
            BitArrayOption::Size {
                location,
                value,
                short_form,
            } => BitArrayOption::Size {
                location,
                value: Box::new(self.constant(*value)),
                short_form,
            },
        }
    }

    fn accessors_map(&mut self, accessors: AccessorsMap) -> AccessorsMap {
        let AccessorsMap {
            publicity,
            type_,
            shared_accessors,
            variant_specific_accessors,
            variant_positional_accessors,
        } = accessors;

        let type_ = self.type_(type_);
        let shared_accessors = shared_accessors
            .into_iter()
            .map(|(label, accessor)| (label, self.record_accessor(accessor)))
            .collect();
        let variant_specific_accessors = variant_specific_accessors
            .into_iter()
            .map(|variant_accessors| {
                variant_accessors
                    .into_iter()
                    .map(|(label, accessor)| (label, self.record_accessor(accessor)))
                    .collect()
            })
            .collect();

        let variant_positional_accessors = variant_positional_accessors
            .into_iter()
            .map(|variant_accessors| {
                variant_accessors
                    .into_iter()
                    .map(|type_| self.type_(type_))
                    .collect()
            })
            .collect();

        AccessorsMap {
            publicity,
            type_,
            shared_accessors,
            variant_specific_accessors,
            variant_positional_accessors,
        }
    }

    fn record_accessor(&mut self, accessor: RecordAccessor) -> RecordAccessor {
        let RecordAccessor {
            index,
            label,
            type_,
            documentation,
        } = accessor;
        RecordAccessor {
            index,
            label,
            type_: self.type_(type_),
            documentation,
        }
    }

    fn type_alias(&mut self, type_: TypeAliasConstructor) -> TypeAliasConstructor {
        let TypeAliasConstructor {
            publicity,
            module,
            type_,
            arity,
            deprecation,
            documentation,
            origin,
            parameters,
        } = type_;

        let type_ = self.type_(type_);
        let parameters = parameters
            .into_iter()
            .map(|parameter| self.type_(parameter))
            .collect();

        TypeAliasConstructor {
            publicity,
            module,
            type_,
            arity,
            deprecation,
            documentation,
            origin,
            parameters,
        }
    }

    fn type_(&mut self, type_: Arc<Type>) -> Arc<Type> {
        let type_ = match Arc::unwrap_or_clone(type_) {
            Type::Named {
                publicity,
                package,
                module,
                name,
                arguments,
                inferred_variant,
            } => Type::Named {
                publicity,
                package,
                module,
                name,
                arguments: arguments
                    .into_iter()
                    .map(|argument| self.type_(argument))
                    .collect(),
                inferred_variant,
            },
            Type::Fn { arguments, return_ } => Type::Fn {
                arguments: arguments
                    .into_iter()
                    .map(|argument| self.type_(argument))
                    .collect(),
                return_: self.type_(return_),
            },
            Type::Var { type_ } => return self.type_var(&type_.borrow()),

            Type::Tuple { elements } => Type::Tuple {
                elements: elements
                    .into_iter()
                    .map(|element| self.type_(element))
                    .collect(),
            },
        };
        Arc::new(type_)
    }

    fn type_var(&mut self, variable: &TypeVar) -> Arc<Type> {
        match variable {
            TypeVar::Link { type_ } => self.type_(type_.clone()),
            TypeVar::Unbound { id } => type_::prelude::unbound_var(self.id(*id)),
            TypeVar::Generic { id } => type_::prelude::generic_var(self.id(*id)),
        }
    }

    fn id(&mut self, id: u64) -> u64 {
        match self.remapped_variable_ids.get(&id) {
            Some(new_id) => *new_id,
            None => {
                let new_id = self.ids.next();
                _ = self.remapped_variable_ids.insert(id, new_id);
                new_id
            }
        }
    }
}
