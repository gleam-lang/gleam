// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2020 The Gleam contributors

use super::*;
use crate::analyse::Inferred;
use crate::type_::{FieldMap, HasType};

pub type TypedConstant = Constant<Arc<Type>>;
pub type UntypedConstant = Constant<()>;

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Constant<T> {
    Int {
        location: SrcSpan,
        value: EcoString,
        int_value: BigInt,
    },

    Float {
        location: SrcSpan,
        value: EcoString,
        float_value: LiteralFloatValue,
    },

    String {
        location: SrcSpan,
        value: EcoString,
    },

    Tuple {
        location: SrcSpan,
        elements: Vec<Self>,
        type_: T,
    },

    List {
        location: SrcSpan,
        elements: Vec<Self>,
        type_: T,
        tail: Option<Box<Self>>,
    },

    Record {
        location: SrcSpan,
        module: Option<(EcoString, SrcSpan)>,
        name: EcoString,
        /// These are the arguments used when calling the record.
        /// If the record is not being called to build a value, then this will
        /// be `None`. A couple of examples:
        /// ```gleam
        /// pub const a = Wibble
        /// // arguments: None
        ///
        /// pub const b = Wibble()
        /// // arguments: Some(vec![])
        ///
        /// pub const c = Wibble(1, 2)
        /// // arguments: Some(vec![1, 2])
        /// ```
        arguments: Option<Vec<CallArg<Self>>>,
        type_: T,
        field_map: Inferred<FieldMap>,
        record_constructor: Option<Box<ValueConstructor>>,
    },

    RecordUpdate {
        location: SrcSpan,
        constructor_location: SrcSpan,
        module: Option<(EcoString, SrcSpan)>,
        name: EcoString,
        record: RecordBeingUpdated<Self>,
        arguments: Vec<RecordUpdateArg<Self>>,
        type_: T,
        field_map: Inferred<FieldMap>,
    },

    BitArray {
        location: SrcSpan,
        segments: Vec<BitArraySegment<Self, T>>,
    },

    Var {
        location: SrcSpan,
        module: Option<(EcoString, SrcSpan)>,
        name: EcoString,
        constructor: Option<Box<ValueConstructor>>,
        type_: T,
    },

    StringConcatenation {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    /// A placeholder constant used to allow module analysis to continue
    /// even when there are type errors. Should never end up in generated code.
    Invalid {
        location: SrcSpan,
        type_: T,
        /// Extra information about the invalid expression, useful for providing
        /// addition help or information, such as code actions to fix invalid
        /// states.
        extra_information: Option<InvalidExpression>,
    },

    Todo {
        location: SrcSpan,
        type_: T,
        message: Option<Box<Self>>,
    },
}

impl TypedConstant {
    pub fn type_(&self) -> Arc<Type> {
        match self {
            Constant::Int { .. } => type_::int(),
            Constant::Float { .. } => type_::float(),
            Constant::String { .. } | Constant::StringConcatenation { .. } => type_::string(),
            Constant::BitArray { .. } => type_::bit_array(),

            Constant::List { type_, .. }
            | Constant::Tuple { type_, .. }
            | Constant::Record { type_, .. }
            | Constant::RecordUpdate { type_, .. }
            | Constant::Var { type_, .. }
            | Constant::Todo { type_, .. }
            | Constant::Invalid { type_, .. } => type_.clone(),
        }
    }

    pub fn find_node(&self, byte_index: u32) -> Option<Located<'_>> {
        if !self.location().contains(byte_index) {
            return None;
        }
        Some(match self {
            Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::String { .. }
            | Constant::Invalid { .. } => Located::Constant(self),

            Constant::Todo { message, .. } => message
                .iter()
                .find_map(|message| message.find_node(byte_index))
                .unwrap_or(Located::Constant(self)),

            Constant::Var {
                module: Some((module_alias, location)),
                constructor: Some(constructor),
                ..
            }
            | Constant::Record {
                module: Some((module_alias, location)),
                record_constructor: Some(constructor),
                ..
            } if location.contains(byte_index) => match &constructor.variant {
                ValueConstructorVariant::ModuleConstant { module, .. }
                | ValueConstructorVariant::ModuleFn { module, .. }
                | ValueConstructorVariant::Record { module, .. } => Located::ModuleName {
                    location: *location,
                    module_name: module.clone(),
                    module_alias: module_alias.clone(),
                    layer: Layer::Value,
                },
                ValueConstructorVariant::LocalVariable { .. } => Located::Constant(self),
            },
            Constant::Var { .. } => Located::Constant(self),
            Constant::Tuple { elements, .. } => elements
                .iter()
                .find_map(|element| element.find_node(byte_index))
                .unwrap_or(Located::Constant(self)),
            Constant::List { elements, tail, .. } => elements
                .iter()
                .find_map(|element| element.find_node(byte_index))
                .or_else(|| tail.as_deref().and_then(|tail| tail.find_node(byte_index)))
                .unwrap_or(Located::Constant(self)),
            Constant::Record {
                arguments,
                type_,
                name,
                ..
            } => arguments
                .iter()
                .flatten()
                .find_map(|argument| argument.find_node(byte_index, type_, name))
                .unwrap_or(Located::Constant(self)),
            Constant::RecordUpdate {
                record, arguments, ..
            } => record
                .base
                .find_node(byte_index)
                .or_else(|| {
                    arguments
                        .iter()
                        .find_map(|arg| arg.value.find_node(byte_index))
                })
                .unwrap_or(Located::Constant(self)),
            Constant::BitArray { segments, .. } => segments
                .iter()
                .find_map(|segment| segment.find_node(byte_index))
                .unwrap_or(Located::Constant(self)),
            Constant::StringConcatenation { left, right, .. } => left
                .find_node(byte_index)
                .or_else(|| right.find_node(byte_index))
                .unwrap_or(Located::Constant(self)),
        })
    }

    pub fn definition_location(&self) -> Option<DefinitionLocation> {
        match self {
            Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::String { .. }
            | Constant::Tuple { .. }
            | Constant::List { .. }
            | Constant::BitArray { .. }
            | Constant::Todo { .. }
            | Constant::StringConcatenation { .. }
            | Constant::Invalid { .. } => None,
            Constant::Record {
                record_constructor: value_constructor,
                ..
            }
            | Constant::Var {
                constructor: value_constructor,
                ..
            } => value_constructor
                .as_ref()
                .map(|constructor| constructor.definition_location()),
            Constant::RecordUpdate { .. } => None,
        }
    }

    pub(crate) fn referenced_variables(&self) -> im::HashSet<&EcoString> {
        match self {
            Constant::Var { name, .. } => im::hashset![name],

            Constant::Invalid { .. }
            | Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::String { .. } => im::hashset![],

            Constant::Todo { message, .. } => message
                .as_ref()
                .map(|message| message.referenced_variables())
                .unwrap_or(im::hashset![]),

            Constant::Tuple { elements, .. } => elements
                .iter()
                .map(|element| element.referenced_variables())
                .fold(im::hashset![], im::HashSet::union),

            Constant::List { elements, tail, .. } => elements
                .iter()
                .map(|element| element.referenced_variables())
                .chain(tail.iter().map(|tail| tail.referenced_variables()))
                .fold(im::hashset![], im::HashSet::union),

            Constant::Record { arguments, .. } => arguments
                .iter()
                .flatten()
                .map(|argument| argument.value.referenced_variables())
                .fold(im::hashset![], im::HashSet::union),

            Constant::RecordUpdate {
                record, arguments, ..
            } => record.base.referenced_variables().union(
                arguments
                    .iter()
                    .map(|arg| arg.value.referenced_variables())
                    .fold(im::hashset![], im::HashSet::union),
            ),

            Constant::BitArray { segments, .. } => segments
                .iter()
                .map(|segment| {
                    segment
                        .options
                        .iter()
                        .map(|option| option.referenced_variables())
                        .fold(segment.value.referenced_variables(), im::HashSet::union)
                })
                .fold(im::hashset![], im::HashSet::union),

            Constant::StringConcatenation { left, right, .. } => left
                .referenced_variables()
                .union(right.referenced_variables()),
        }
    }

    pub(crate) fn syntactically_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Constant::Todo { message, .. },
                Constant::Todo {
                    message: other_message,
                    ..
                },
            ) => match (message, other_message) {
                (None, None) => true,
                (Some(_), None) | (None, Some(_)) => false,
                (Some(message), Some(other_message)) => message.syntactically_eq(other_message),
            },
            (Constant::Todo { .. }, _) => false,

            (Constant::Int { int_value: n, .. }, Constant::Int { int_value: m, .. }) => n == m,
            (Constant::Int { .. }, _) => false,

            (Constant::Float { float_value: n, .. }, Constant::Float { float_value: m, .. }) => {
                n == m
            }
            (Constant::Float { .. }, _) => false,

            (
                Constant::String { value, .. },
                Constant::String {
                    value: other_value, ..
                },
            ) => value == other_value,
            (Constant::String { .. }, _) => false,

            (
                Constant::Tuple { elements, .. },
                Constant::Tuple {
                    elements: other_elements,
                    ..
                },
            ) => pairwise_all(elements, other_elements, |(one, other)| {
                one.syntactically_eq(other)
            }),
            (Constant::Tuple { .. }, _) => false,

            (
                Constant::List { elements, tail, .. },
                Constant::List {
                    elements: other_elements,
                    tail: other_tail,
                    ..
                },
            ) => {
                let tails_are_equal = match (tail, other_tail) {
                    (None, None) => true,
                    (None, Some(_)) | (Some(_), None) => false,
                    (Some(tail), Some(other_tail)) => tail.syntactically_eq(other_tail),
                };
                tails_are_equal
                    && pairwise_all(elements, other_elements, |(one, other)| {
                        one.syntactically_eq(other)
                    })
            }
            (Constant::List { .. }, _) => false,

            (
                Constant::Record {
                    module,
                    name,
                    arguments,
                    ..
                },
                Constant::Record {
                    module: other_module,
                    name: other_name,
                    arguments: other_arguments,
                    ..
                },
            ) => {
                let modules_are_equal = match (module, other_module) {
                    (None, None) => true,
                    (None, Some(_)) | (Some(_), None) => false,
                    (Some((one, _)), Some((other, _))) => one == other,
                };

                let arguments_are_equal = match (arguments, other_arguments) {
                    (None, None) => true,
                    (None, Some(_)) | (Some(_), None) => false,
                    (Some(arguments), Some(other_arguments)) => {
                        modules_are_equal
                            && name == other_name
                            && pairwise_all(arguments, other_arguments, |(one, other)| {
                                one.label == other.label && one.value.syntactically_eq(&other.value)
                            })
                    }
                };

                modules_are_equal && arguments_are_equal
            }
            (Constant::Record { .. }, _) => false,

            (
                Constant::RecordUpdate {
                    module,
                    name,
                    record,
                    arguments,
                    ..
                },
                Constant::RecordUpdate {
                    module: other_module,
                    name: other_name,
                    record: other_record,
                    arguments: other_arguments,
                    ..
                },
            ) => {
                let modules_are_equal = match (module, other_module) {
                    (None, None) => true,
                    (None, Some(_)) | (Some(_), None) => false,
                    (Some((one, _)), Some((other, _))) => one == other,
                };

                modules_are_equal
                    && name == other_name
                    && record.base.syntactically_eq(&other_record.base)
                    && pairwise_all(arguments, other_arguments, |(one, other)| {
                        one.label == other.label && one.value.syntactically_eq(&other.value)
                    })
            }
            (Constant::RecordUpdate { .. }, _) => false,

            (
                Constant::BitArray { segments, .. },
                Constant::BitArray {
                    segments: other_segments,
                    ..
                },
            ) => pairwise_all(segments, other_segments, |(one, other)| {
                one.syntactically_eq(other)
            }),
            (Constant::BitArray { .. }, _) => false,

            (
                Constant::Var { module, name, .. },
                Constant::Var {
                    module: other_module,
                    name: other_name,
                    ..
                },
            ) => {
                let modules_are_equal = match (module, other_module) {
                    (None, None) => true,
                    (None, Some(_)) | (Some(_), None) => false,
                    (Some((one, _)), Some((other, _))) => one == other,
                };

                modules_are_equal && name == other_name
            }
            (Constant::Var { .. }, _) => false,

            (
                Constant::StringConcatenation { left, right, .. },
                Constant::StringConcatenation {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => left.syntactically_eq(other_left) && right.syntactically_eq(other_right),
            (Constant::StringConcatenation { .. }, _) => false,

            (Constant::Invalid { .. }, _) => false,
        }
    }

    /// If the constant is a list whose elements are known at compile time, this
    /// returns a vec of all its elements.
    /// This can happen with:
    /// - literal lists: `[1, 2]`
    /// - literal lists whose tail is also known at compile time:
    ///     - `[1, 2, ..[3, 4]]`
    ///     - `[1, 2, ..a_known_module_constant]`
    ///
    pub fn list_elements(&self) -> Option<Vec<&Self>> {
        match self {
            Constant::List { elements, tail, .. } => {
                if let Some(tail) = tail {
                    // There's a tail, if it cannot be known at compile time,
                    // then this entire list cannot be known at compile time!
                    let tail_elements = tail.list_elements()?;
                    Some(elements.iter().chain(tail_elements).collect())
                } else {
                    // There's no tail, we just return the elements
                    Some(elements.iter().collect())
                }
            }
            Constant::Var {
                constructor: Some(constructor),
                ..
            } => match &constructor.variant {
                ValueConstructorVariant::ModuleConstant { literal, .. } => literal.list_elements(),
                ValueConstructorVariant::LocalVariable { .. }
                | ValueConstructorVariant::ModuleFn { .. }
                | ValueConstructorVariant::Record { .. } => None,
            },

            Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::String { .. }
            | Constant::Tuple { .. }
            | Constant::Record { .. }
            | Constant::RecordUpdate { .. }
            | Constant::BitArray { .. }
            | Constant::StringConcatenation { .. }
            | Constant::Var { .. }
            | Constant::Todo { .. }
            | Constant::Invalid { .. } => None,
        }
    }

    /// If the constant is a record or record update this returns its tag.
    /// It might return `None` if the record constructor couldn't be inferred.
    /// For example, if someone wrote a variant that doesn't exist:
    ///
    /// ```gleam
    /// pub const wibble = ThisIsNotDefinedAnywhere(1, 2)
    /// ```
    ///
    /// In this case the record wouldn't have a constructor, as there's no
    /// custom type defining it anywhere!
    ///
    pub(crate) fn constant_record_tag(&self) -> Option<EcoString> {
        if let Constant::Record {
            record_constructor: Some(constructor),
            ..
        } = self
            && let ValueConstructorVariant::Record { name, .. } = &constructor.variant
        {
            Some(name.clone())
        } else if let Constant::RecordUpdate { name, .. } = self {
            Some(name.clone())
        } else {
            None
        }
    }
}

impl HasType for TypedConstant {
    fn type_(&self) -> Arc<Type> {
        self.type_()
    }
}

impl<A> Constant<A> {
    pub fn location(&self) -> SrcSpan {
        match self {
            Constant::Int { location, .. }
            | Constant::List { location, .. }
            | Constant::Float { location, .. }
            | Constant::Tuple { location, .. }
            | Constant::String { location, .. }
            | Constant::Record { location, .. }
            | Constant::RecordUpdate { location, .. }
            | Constant::BitArray { location, .. }
            | Constant::Var { location, .. }
            | Constant::Invalid { location, .. }
            | Constant::Todo { location, .. }
            | Constant::StringConcatenation { location, .. } => *location,
        }
    }

    #[must_use]
    pub fn can_have_multiple_per_line(&self) -> bool {
        match self {
            Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::String { .. }
            | Constant::Var { .. } => true,

            Constant::Tuple { .. }
            | Constant::Todo { .. }
            | Constant::List { .. }
            | Constant::Record { .. }
            | Constant::RecordUpdate { .. }
            | Constant::BitArray { .. }
            | Constant::StringConcatenation { .. }
            | Constant::Invalid { .. } => false,
        }
    }
}

impl<A> HasLocation for Constant<A> {
    fn location(&self) -> SrcSpan {
        self.location()
    }
}

impl<A> bit_array::GetLiteralValue for Constant<A> {
    fn as_int_literal(&self) -> Option<BigInt> {
        if let Constant::Int { int_value, .. } = self {
            Some(int_value.clone())
        } else {
            None
        }
    }
}
