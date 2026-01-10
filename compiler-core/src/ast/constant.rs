use super::*;
use crate::analyse::Inferred;
use crate::type_::{FieldMap, HasType};

pub type TypedConstant = Constant<Arc<Type>, EcoString>;
pub type UntypedConstant = Constant<(), ()>;

// TODO: remove RecordTag paramter
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constant<T, RecordTag> {
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
    },

    Record {
        location: SrcSpan,
        module: Option<(EcoString, SrcSpan)>,
        name: EcoString,
        arguments: Vec<CallArg<Self>>,
        tag: RecordTag,
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
        tag: RecordTag,
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
            Constant::Tuple { elements, .. } | Constant::List { elements, .. } => elements
                .iter()
                .find_map(|element| element.find_node(byte_index))
                .unwrap_or(Located::Constant(self)),
            Constant::Record { arguments, .. } => arguments
                .iter()
                .find_map(|argument| argument.find_node(byte_index))
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

            Constant::List { elements, .. } | Constant::Tuple { elements, .. } => elements
                .iter()
                .map(|element| element.referenced_variables())
                .fold(im::hashset![], im::HashSet::union),

            Constant::Record { arguments, .. } => arguments
                .iter()
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
                Constant::List { elements, .. },
                Constant::List {
                    elements: other_elements,
                    ..
                },
            ) => pairwise_all(elements, other_elements, |(one, other)| {
                one.syntactically_eq(other)
            }),
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

                modules_are_equal
                    && name == other_name
                    && pairwise_all(arguments, other_arguments, |(one, other)| {
                        one.label == other.label && one.value.syntactically_eq(&other.value)
                    })
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
}

impl HasType for TypedConstant {
    fn type_(&self) -> Arc<Type> {
        self.type_()
    }
}

impl<A, B> Constant<A, B> {
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
            | Constant::List { .. }
            | Constant::Record { .. }
            | Constant::RecordUpdate { .. }
            | Constant::BitArray { .. }
            | Constant::StringConcatenation { .. }
            | Constant::Invalid { .. } => false,
        }
    }
}

impl<A, B> HasLocation for Constant<A, B> {
    fn location(&self) -> SrcSpan {
        self.location()
    }
}

impl<A, B> bit_array::GetLiteralValue for Constant<A, B> {
    fn as_int_literal(&self) -> Option<BigInt> {
        if let Constant::Int { int_value, .. } = self {
            Some(int_value.clone())
        } else {
            None
        }
    }
}
