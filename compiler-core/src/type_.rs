pub(crate) mod environment;
pub(crate) mod error;
pub(crate) mod expression;
pub(crate) mod fields;
pub(crate) mod hydrator;
pub(crate) mod pattern;
pub(crate) mod pipe;
pub(crate) mod prelude;
pub mod pretty;
pub(crate) mod printer;
#[cfg(test)]
pub mod tests;

use camino::Utf8PathBuf;
use ecow::EcoString;
pub use environment::*;
pub use error::{Error, Problems, UnifyErrorSituation, Warning};
pub(crate) use expression::ExprTyper;
use expression::Purity;
pub use fields::FieldMap;
use hexpm::version::Version;
pub use prelude::*;
use printer::Names;
use serde::Serialize;

use crate::{
    ast::{
        ArgNames, BitArraySegment, CallArg, Constant, DefinitionLocation, Pattern, Publicity,
        SrcSpan, TypedConstant, TypedExpr, TypedPattern, TypedPatternBitArraySegment,
        UntypedMultiPattern, UntypedPattern, UntypedRecordUpdateArg,
    },
    bit_array,
    build::{Origin, Target},
    line_numbers::LineNumbers,
    reference::ReferenceMap,
    type_::expression::Implementations,
};
use error::*;
use hydrator::Hydrator;
use itertools::Itertools;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::Deref,
    sync::Arc,
};

pub trait HasType {
    fn type_(&self) -> Arc<Type>;
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Type {
    /// A nominal (named) type such as `Int`, `Float`, or a programmer defined
    /// custom type such as `Person`. The type can take other types as
    /// arguments (aka "generics" or "parametric polymorphism").
    ///
    /// If the type is defined in the Gleam prelude the `module` field will be
    /// the string "gleam", otherwise it will contain the name of the module
    /// that defines the type.
    ///
    Named {
        publicity: Publicity,
        package: EcoString,
        module: EcoString,
        name: EcoString,
        args: Vec<Arc<Type>>,

        /// Which variant of the types this value is, if it is known from variant inference.
        /// This allows us to permit certain operations when we know this,
        /// such as record updates for multi-constructor types, or field access
        /// for fields not shared between type variants. For example:
        ///
        /// ```gleam
        /// type Wibble {
        ///   Wibble(wibble: Int, other, Int)
        ///   Wobble(wobble: Int, something: Int)
        /// }
        ///
        /// fn add_one(some_wibble: Wibble) -> Wibble {
        ///   case some_wibble {
        ///     Wibble(..) as wibble -> Wibble(..wibble, other: wibble.other + 1)
        ///     Wobble(..) as wobble -> Wobble(..wobble, something: wobble.something + 1)
        ///   }
        /// }
        /// ```
        ///
        /// Here, the `wibble` variable has an inferred variant of `0`, since we know it's
        /// of the `Wibble` variant. This means we can safely update it using the `Wibble`
        /// constructor, and access the `other` field, which is only present in that variant.
        ///
        /// However, the parameter `some_wibble` has no known variant; it could be either of the variants,
        /// so we can't allow any of that until we pattern match on it.
        ///
        inferred_variant: Option<u16>,
    },

    /// The type of a function. It takes arguments and returns a value.
    ///
    Fn {
        args: Vec<Arc<Type>>,
        return_: Arc<Type>,
    },

    /// A type variable. See the contained `TypeVar` enum for more information.
    ///
    Var { type_: Arc<RefCell<TypeVar>> },

    /// A tuple is an ordered collection of 0 or more values, each of which
    /// can have a different type, so the `tuple` type is the sum of all the
    /// contained types.
    ///
    Tuple { elements: Vec<Arc<Type>> },
}

impl Type {
    pub fn is_result_constructor(&self) -> bool {
        match self {
            Type::Fn { return_, .. } => return_.is_result(),
            Type::Var { type_ } => type_.borrow().is_result(),
            _ => false,
        }
    }

    pub fn is_result(&self) -> bool {
        match self {
            Self::Named { name, module, .. } => "Result" == name && is_prelude_module(module),
            Self::Var { type_ } => type_.borrow().is_result(),
            _ => false,
        }
    }

    pub fn result_ok_type(&self) -> Option<Arc<Type>> {
        match self {
            Self::Named {
                module, name, args, ..
            } if "Result" == name && is_prelude_module(module) => args.first().cloned(),
            Self::Var { type_ } => type_.borrow().result_ok_type(),
            Self::Named { .. } | Self::Tuple { .. } | Type::Fn { .. } => None,
        }
    }

    pub fn result_types(&self) -> Option<(Arc<Type>, Arc<Type>)> {
        match self {
            Self::Named {
                module, name, args, ..
            } if "Result" == name && is_prelude_module(module) => {
                Some((args.first().cloned()?, args.get(1).cloned()?))
            }
            Self::Var { type_ } => type_.borrow().result_types(),
            Self::Named { .. } | Self::Tuple { .. } | Type::Fn { .. } => None,
        }
    }

    pub fn is_unbound(&self) -> bool {
        match self {
            Self::Var { type_ } => type_.borrow().is_unbound(),
            _ => false,
        }
    }

    pub fn is_variable(&self) -> bool {
        match self {
            Self::Var { type_ } => type_.borrow().is_variable(),
            _ => false,
        }
    }

    pub fn return_type(&self) -> Option<Arc<Self>> {
        match self {
            Self::Fn { return_, .. } => Some(return_.clone()),
            Self::Var { type_ } => type_.borrow().return_type(),
            _ => None,
        }
    }

    pub fn fn_types(&self) -> Option<(Vec<Arc<Self>>, Arc<Self>)> {
        match self {
            Self::Fn { args, return_, .. } => Some((args.clone(), return_.clone())),
            Self::Var { type_ } => type_.borrow().fn_types(),
            _ => None,
        }
    }

    /// Gets the types inside of a tuple. Returns `None` if the type is not a tuple.
    pub fn tuple_types(&self) -> Option<Vec<Arc<Self>>> {
        match self {
            Self::Tuple { elements } => Some(elements.clone()),
            Self::Var { type_, .. } => type_.borrow().tuple_types(),
            _ => None,
        }
    }

    /// Gets the argument types for a type constructor. Returns `None` if the type
    /// does not lead to a type constructor.
    pub fn constructor_types(&self) -> Option<Vec<Arc<Self>>> {
        match self {
            Self::Named { args, .. } => Some(args.clone()),
            Self::Var { type_, .. } => type_.borrow().constructor_types(),
            _ => None,
        }
    }

    /// If the type is a Gleam's prelude's List this will return its wrapped
    /// type.
    pub fn list_type(&self) -> Option<Arc<Self>> {
        match self {
            Type::Named {
                publicity: Publicity::Public,
                name,
                module,
                package,
                args,
                inferred_variant: _,
            } if package == PRELUDE_PACKAGE_NAME
                && module == PRELUDE_MODULE_NAME
                && name == LIST =>
            {
                match args.as_slice() {
                    [inner_type] => Some(inner_type.clone()),
                    [] | [_, _, ..] => None,
                }
            }
            _ => None,
        }
    }

    pub fn list(inner_type: Arc<Self>) -> Self {
        Type::Named {
            publicity: Publicity::Public,
            package: PRELUDE_PACKAGE_NAME.into(),
            module: PRELUDE_MODULE_NAME.into(),
            name: LIST.into(),
            args: vec![inner_type],
            inferred_variant: None,
        }
    }

    #[must_use]
    fn is_fun(&self) -> bool {
        match self {
            Self::Fn { .. } => true,
            Type::Var { type_ } => type_.borrow().is_fun(),
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Self::Named { module, name, .. } if "Nil" == name && is_prelude_module(module) => true,
            Self::Var { type_ } => type_.borrow().is_nil(),
            _ => false,
        }
    }

    pub fn is_bit_array(&self) -> bool {
        match self {
            Self::Named { module, name, .. } if "BitArray" == name && is_prelude_module(module) => {
                true
            }
            Self::Var { type_ } => type_.borrow().is_nil(),
            _ => false,
        }
    }

    pub fn is_utf_codepoint(&self) -> bool {
        match self {
            Self::Named { module, name, .. }
                if "UtfCodepoint" == name && is_prelude_module(module) =>
            {
                true
            }
            Self::Var { type_ } => type_.borrow().is_nil(),
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::Named { module, name, .. } if "Bool" == name && is_prelude_module(module) => true,
            Self::Var { type_ } => type_.borrow().is_bool(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Self::Named { module, name, .. } if "Int" == name && is_prelude_module(module) => true,
            Self::Var { type_ } => type_.borrow().is_int(),
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::Named { module, name, .. } if "Float" == name && is_prelude_module(module) => {
                true
            }
            Self::Var { type_ } => type_.borrow().is_float(),
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::Named { module, name, .. } if "String" == name && is_prelude_module(module) => {
                true
            }
            Self::Var { type_ } => type_.borrow().is_string(),
            _ => false,
        }
    }

    pub fn named_type_name(&self) -> Option<(EcoString, EcoString)> {
        match self {
            Self::Named { module, name, .. } => Some((module.clone(), name.clone())),
            Self::Var { type_ } => type_.borrow().named_type_name(),
            _ => None,
        }
    }

    pub fn named_type_information(&self) -> Option<(EcoString, EcoString, Vec<Arc<Self>>)> {
        match self {
            Self::Named {
                module, name, args, ..
            } => Some((module.clone(), name.clone(), args.clone())),
            Self::Var { type_ } => type_.borrow().named_type_information(),
            _ => None,
        }
    }

    pub fn set_custom_type_variant(&mut self, index: u16) {
        match self {
            Type::Named {
                inferred_variant, ..
            } => *inferred_variant = Some(index),
            Type::Var { type_ } => type_.borrow_mut().set_custom_type_variant(index),
            Type::Fn { .. } | Type::Tuple { .. } => {}
        }
    }

    pub fn generalise_custom_type_variant(&mut self) {
        match self {
            Type::Named {
                inferred_variant, ..
            } => *inferred_variant = None,
            Type::Var { type_ } => type_.borrow_mut().generalise_custom_type_variant(),
            Type::Tuple { elements } => {
                for element in elements {
                    Arc::make_mut(element).generalise_custom_type_variant();
                }
            }
            Type::Fn { args, return_ } => {
                for argument in args {
                    Arc::make_mut(argument).generalise_custom_type_variant();
                }
                Arc::make_mut(return_).generalise_custom_type_variant();
            }
        }
    }

    pub fn custom_type_inferred_variant(&self) -> Option<u16> {
        match self {
            Type::Named {
                inferred_variant, ..
            } => *inferred_variant,
            Type::Var { type_ } => type_.borrow().custom_type_inferred_variant(),
            Type::Fn { .. } | Type::Tuple { .. } => None,
        }
    }

    /// Get the args for the type if the type is a specific `Type::App`.
    /// Returns None if the type is not a `Type::App` or is an incorrect `Type:App`
    ///
    /// This function is currently only used for finding the `List` type.
    ///
    // TODO: specialise this to just List.
    pub fn get_app_args(
        &self,
        publicity: Publicity,
        package: &str,
        module: &str,
        name: &str,
        arity: usize,
        environment: &mut Environment<'_>,
    ) -> Option<Vec<Arc<Self>>> {
        match self {
            Self::Named {
                module: m,
                name: n,
                args,
                ..
            } => {
                if module == m && name == n && args.len() == arity {
                    Some(args.clone())
                } else {
                    None
                }
            }

            Self::Var { type_ } => {
                let args: Vec<_> = match type_.borrow().deref() {
                    TypeVar::Link { type_ } => {
                        return type_.get_app_args(
                            publicity,
                            package,
                            module,
                            name,
                            arity,
                            environment,
                        );
                    }

                    TypeVar::Unbound { .. } => {
                        (0..arity).map(|_| environment.new_unbound_var()).collect()
                    }

                    TypeVar::Generic { .. } => return None,
                };

                // We are an unbound type variable! So convert us to a type link
                // to the desired type.
                *type_.borrow_mut() = TypeVar::Link {
                    type_: Arc::new(Self::Named {
                        name: name.into(),
                        package: package.into(),
                        module: module.into(),
                        args: args.clone(),
                        publicity,
                        inferred_variant: None,
                    }),
                };
                Some(args)
            }

            _ => None,
        }
    }

    pub fn find_private_type(&self) -> Option<Self> {
        match self {
            Self::Named {
                publicity: Publicity::Private,
                ..
            } => Some(self.clone()),

            Self::Named { args, .. } => args.iter().find_map(|type_| type_.find_private_type()),

            Self::Tuple { elements, .. } => {
                elements.iter().find_map(|type_| type_.find_private_type())
            }

            Self::Fn { return_, args, .. } => return_
                .find_private_type()
                .or_else(|| args.iter().find_map(|type_| type_.find_private_type())),

            Self::Var { type_, .. } => match type_.borrow().deref() {
                TypeVar::Unbound { .. } => None,

                TypeVar::Generic { .. } => None,

                TypeVar::Link { type_, .. } => type_.find_private_type(),
            },
        }
    }

    pub fn find_internal_type(&self) -> Option<Self> {
        match self {
            Self::Named { publicity, .. } if publicity.is_internal() => Some(self.clone()),

            Self::Named { args, .. } => args.iter().find_map(|type_| type_.find_internal_type()),

            Self::Tuple { elements, .. } => {
                elements.iter().find_map(|type_| type_.find_internal_type())
            }

            Self::Fn { return_, args, .. } => return_
                .find_internal_type()
                .or_else(|| args.iter().find_map(|type_| type_.find_internal_type())),

            Self::Var { type_, .. } => match type_.borrow().deref() {
                TypeVar::Unbound { .. } | TypeVar::Generic { .. } => None,
                TypeVar::Link { type_, .. } => type_.find_internal_type(),
            },
        }
    }

    pub fn fn_arity(&self) -> Option<usize> {
        match self {
            Self::Fn { args, .. } => Some(args.len()),
            _ => None,
        }
    }

    #[must_use]
    /// Returns `true` is the two types are the same. This differs from the
    /// standard `Eq` implementation as it also follows all links to check if
    /// two types are really the same.
    ///
    pub fn same_as(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Named { .. }, Type::Fn { .. } | Type::Tuple { .. }) => false,
            (one @ Type::Named { .. }, Type::Var { type_ }) => {
                type_.as_ref().borrow().same_as_other_type(one)
            }
            // When comparing two types we don't care about the inferred variant:
            // `True` has the same type as `False`, even if the inferred variants
            // differ.
            (
                Type::Named {
                    publicity,
                    package,
                    module,
                    name,
                    args,
                    inferred_variant: _,
                },
                Type::Named {
                    publicity: other_publicity,
                    package: other_package,
                    module: other_module,
                    name: other_name,
                    args: other_args,
                    inferred_variant: _,
                },
            ) => {
                publicity == other_publicity
                    && package == other_package
                    && module == other_module
                    && name == other_name
                    && args == other_args
            }

            (Type::Fn { .. }, Type::Named { .. } | Type::Tuple { .. }) => false,
            (one @ Type::Fn { .. }, Type::Var { type_ }) => {
                type_.as_ref().borrow().same_as_other_type(one)
            }
            (
                Type::Fn { args, return_ },
                Type::Fn {
                    args: other_args,
                    return_: other_return,
                },
            ) => {
                args.len() == other_args.len()
                    && args
                        .iter()
                        .zip(other_args)
                        .all(|(one, other)| one.same_as(other))
                    && return_.same_as(other_return)
            }

            (Type::Var { type_ }, other) => type_.as_ref().borrow().same_as_other_type(other),

            (Type::Tuple { .. }, Type::Fn { .. } | Type::Named { .. }) => false,
            (one @ Type::Tuple { .. }, Type::Var { type_ }) => {
                type_.as_ref().borrow().same_as_other_type(one)
            }
            (
                Type::Tuple { elements },
                Type::Tuple {
                    elements: other_elements,
                },
            ) => {
                elements.len() == other_elements.len()
                    && elements
                        .iter()
                        .zip(other_elements)
                        .all(|(one, other)| one.same_as(other))
            }
        }
    }

    pub(crate) fn is_non_public_named_type(&self) -> bool {
        match self {
            Type::Named { publicity, .. } => match publicity {
                Publicity::Public => false,
                Publicity::Private => true,
                Publicity::Internal { .. } => true,
            },
            Type::Fn { .. } => false,
            Type::Var { .. } => false,
            Type::Tuple { .. } => false,
        }
    }
}

impl TypeVar {
    #[must_use]
    fn same_as_other_type(&self, other: &Type) -> bool {
        match (self, other) {
            (TypeVar::Unbound { .. }, _) => true,
            (TypeVar::Link { type_ }, other) => type_.same_as(other),

            (
                TypeVar::Generic { .. },
                Type::Named { .. } | Type::Fn { .. } | Type::Tuple { .. },
            ) => false,

            (one @ TypeVar::Generic { .. }, Type::Var { type_ }) => {
                one.same_as(&type_.as_ref().borrow())
            }
        }
    }

    #[must_use]
    fn same_as(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeVar::Unbound { .. }, _) | (_, TypeVar::Unbound { .. }) => true,
            (TypeVar::Link { type_ }, TypeVar::Link { type_: other_type }) => {
                type_.same_as(other_type)
            }
            (TypeVar::Link { type_ }, other @ TypeVar::Generic { .. }) => {
                other.same_as_other_type(type_)
            }
            (TypeVar::Generic { id }, TypeVar::Generic { id: other_id }) => id == other_id,
            (one @ TypeVar::Generic { .. }, TypeVar::Link { type_ }) => {
                one.same_as_other_type(type_)
            }
        }
    }
}

pub fn collapse_links(t: Arc<Type>) -> Arc<Type> {
    if let Type::Var { type_ } = t.deref() {
        if let TypeVar::Link { type_ } = type_.borrow().deref() {
            return collapse_links(type_.clone());
        }
    }
    t
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AccessorsMap {
    pub publicity: Publicity,
    pub type_: Arc<Type>,
    pub shared_accessors: HashMap<EcoString, RecordAccessor>,
    pub variant_specific_accessors: Vec<HashMap<EcoString, RecordAccessor>>,
}

impl AccessorsMap {
    pub fn accessors_for_variant(
        &self,
        inferred_variant: Option<u16>,
    ) -> &HashMap<EcoString, RecordAccessor> {
        inferred_variant
            .and_then(|index| self.variant_specific_accessors.get(index as usize))
            .unwrap_or(&self.shared_accessors)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordAccessor {
    // TODO: smaller int. Doesn't need to be this big
    pub index: u64,
    pub label: EcoString,
    pub type_: Arc<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueConstructorVariant {
    /// A locally defined variable or function parameter
    LocalVariable {
        location: SrcSpan,
        origin: VariableOrigin,
    },

    /// A module constant
    ModuleConstant {
        documentation: Option<EcoString>,
        location: SrcSpan,
        module: EcoString,
        name: EcoString,
        literal: Constant<Arc<Type>, EcoString>,
        implementations: Implementations,
    },

    /// A constant defined locally, for example when pattern matching on string literals
    LocalConstant {
        literal: Constant<Arc<Type>, EcoString>,
    },

    /// A function belonging to the module
    ModuleFn {
        name: EcoString,
        field_map: Option<FieldMap>,
        module: EcoString,
        arity: usize,
        location: SrcSpan,
        documentation: Option<EcoString>,
        implementations: Implementations,
        external_erlang: Option<(EcoString, EcoString)>,
        external_javascript: Option<(EcoString, EcoString)>,
        purity: Purity,
    },

    /// A constructor for a custom type
    Record {
        name: EcoString,
        arity: u16,
        field_map: Option<FieldMap>,
        location: SrcSpan,
        module: EcoString,
        variants_count: u16,
        variant_index: u16,
        documentation: Option<EcoString>,
    },
}

impl ValueConstructorVariant {
    fn to_module_value_constructor(
        &self,
        type_: Arc<Type>,
        module_name: &EcoString,
        function_name: &EcoString,
    ) -> ModuleValueConstructor {
        match self {
            Self::Record {
                name,
                arity,
                field_map,
                location,
                documentation,
                ..
            } => ModuleValueConstructor::Record {
                name: name.clone(),
                field_map: field_map.clone(),
                arity: *arity,
                type_,
                location: *location,
                documentation: documentation.clone(),
            },

            // TODO: remove this clone with an rc clone
            Self::ModuleConstant {
                documentation,
                literal,
                location,
                ..
            } => ModuleValueConstructor::Constant {
                literal: literal.clone(),
                location: *location,
                documentation: documentation.clone(),
            },

            Self::LocalConstant { literal } => ModuleValueConstructor::Constant {
                literal: literal.clone(),
                location: literal.location(),
                documentation: None,
            },

            Self::LocalVariable { location, .. } => ModuleValueConstructor::Fn {
                name: function_name.clone(),
                module: module_name.clone(),
                external_erlang: None,
                external_javascript: None,
                documentation: None,
                location: *location,
                field_map: None,
                purity: Purity::Impure,
            },

            Self::ModuleFn {
                name,
                module,
                location,
                documentation,
                field_map,
                external_erlang,
                external_javascript,
                purity,
                ..
            } => ModuleValueConstructor::Fn {
                name: name.clone(),
                module: module.clone(),
                documentation: documentation.clone(),
                external_erlang: external_erlang.clone(),
                external_javascript: external_javascript.clone(),
                location: *location,
                field_map: field_map.clone(),
                purity: *purity,
            },
        }
    }

    pub fn definition_location(&self) -> SrcSpan {
        match self {
            ValueConstructorVariant::LocalVariable { location, .. }
            | ValueConstructorVariant::ModuleConstant { location, .. }
            | ValueConstructorVariant::ModuleFn { location, .. }
            | ValueConstructorVariant::Record { location, .. } => *location,
            ValueConstructorVariant::LocalConstant { literal } => literal.location(),
        }
    }

    /// Returns `true` if the variant is [`LocalVariable`].
    pub fn is_local_variable(&self) -> bool {
        matches!(self, Self::LocalVariable { .. })
    }

    /// Returns `true` if the variant is a local variable generated by the compiler.
    #[must_use]
    pub fn is_generated_variable(&self) -> bool {
        match self {
            ValueConstructorVariant::LocalVariable {
                origin: VariableOrigin::Generated,
                ..
            } => true,
            ValueConstructorVariant::LocalVariable { .. } => false,
            ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::LocalConstant { .. }
            | ValueConstructorVariant::ModuleFn { .. }
            | ValueConstructorVariant::Record { .. } => false,
        }
    }

    /// Returns `true` if the value constructor variant is [`ModuleFn`].
    ///
    /// [`ModuleFn`]: ValueConstructorVariant::ModuleFn
    #[must_use]
    pub fn is_module_fn(&self) -> bool {
        matches!(self, Self::ModuleFn { .. })
    }

    pub fn is_record(&self) -> bool {
        match self {
            Self::Record { .. } => true,
            _ => false,
        }
    }

    pub fn implementations(&self) -> Implementations {
        match self {
            ValueConstructorVariant::Record { .. }
            | ValueConstructorVariant::LocalConstant { .. }
            | ValueConstructorVariant::LocalVariable { .. } => Implementations {
                gleam: true,
                can_run_on_erlang: true,
                can_run_on_javascript: true,
                uses_javascript_externals: false,
                uses_erlang_externals: false,
            },

            ValueConstructorVariant::ModuleFn {
                implementations, ..
            }
            | ValueConstructorVariant::ModuleConstant {
                implementations, ..
            } => *implementations,
        }
    }

    fn record_field_map(&self) -> Option<&FieldMap> {
        match self {
            ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::LocalConstant { .. }
            | ValueConstructorVariant::ModuleFn { .. } => None,
            ValueConstructorVariant::Record { field_map, .. } => field_map.as_ref(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleValueConstructor {
    Record {
        name: EcoString,
        arity: u16,
        type_: Arc<Type>,
        field_map: Option<FieldMap>,
        location: SrcSpan,
        documentation: Option<EcoString>,
    },

    Fn {
        location: SrcSpan,
        /// The name of the module and the function
        /// This will be the module that this constructor belongs to
        /// and the name that was used for the function.
        module: EcoString,
        name: EcoString,
        /// If this is an `external` function, these will hold the name of the
        /// external module and function.
        ///
        /// This function has module "themodule" and name "wibble"
        ///     pub fn wibble() { Nil }
        ///
        /// This function has module "themodule" and name "wibble"
        /// and erlang external "other" and "whoop".
        ///     @external(erlang, "other", "whoop")
        ///     pub fn wibble() -> Nil
        ///
        external_erlang: Option<(EcoString, EcoString)>,
        external_javascript: Option<(EcoString, EcoString)>,
        field_map: Option<FieldMap>,
        documentation: Option<EcoString>,
        purity: Purity,
    },

    Constant {
        literal: TypedConstant,
        location: SrcSpan,
        documentation: Option<EcoString>,
    },
}

impl ModuleValueConstructor {
    pub fn location(&self) -> SrcSpan {
        match self {
            ModuleValueConstructor::Fn { location, .. }
            | ModuleValueConstructor::Record { location, .. }
            | ModuleValueConstructor::Constant { location, .. } => *location,
        }
    }

    pub fn get_documentation(&self) -> Option<&str> {
        match self {
            ModuleValueConstructor::Record { documentation, .. }
            | ModuleValueConstructor::Fn { documentation, .. }
            | ModuleValueConstructor::Constant { documentation, .. } => documentation.as_deref(),
        }
    }

    /// Returns the purity of this value constructor if it is called as a function.
    /// Referencing a module value by itself is always pure, but calling is as a
    /// function might not be.
    pub fn called_function_purity(&self) -> Purity {
        match self {
            // If we call a module constant or local variable as a function, we
            // no longer have enough information to determine its purity. For
            // example:
            //
            // ```gleam
            // const function1 = io.println
            // const function2 = function.identity
            //
            // pub fn main() {
            //   function1("Hello")
            //   function2("Hello")
            // }
            // ```
            //
            // At this point, we don't have any information about the purity of
            // the `function1` and `function2` functions, and must return
            // `Purity::Unknown`. See the documentation for the `Purity` type
            // for more information on why this is the case.
            ModuleValueConstructor::Constant { .. } => Purity::Unknown,

            // Constructing records is always pure
            ModuleValueConstructor::Record { .. } => Purity::Pure,

            ModuleValueConstructor::Fn { purity, .. } => *purity,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleFunction {
    pub package: EcoString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleInterface {
    pub name: EcoString,
    pub origin: Origin,
    pub package: EcoString,
    pub types: HashMap<EcoString, TypeConstructor>,
    pub types_value_constructors: HashMap<EcoString, TypeVariantConstructors>,
    pub values: HashMap<EcoString, ValueConstructor>,
    pub accessors: HashMap<EcoString, AccessorsMap>,
    /// Used for mapping to original source locations on disk
    pub line_numbers: LineNumbers,
    /// Used for determining the source path of the module on disk
    pub src_path: Utf8PathBuf,
    /// Whether the module is internal or not. Internal modules are technically
    /// importable by other packages but to do so is violating the contract of
    /// the package and as such is not recommended.
    pub is_internal: bool,
    /// Warnings emitted during analysis of this module.
    pub warnings: Vec<Warning>,
    /// The minimum Gleam version needed to use this module.
    pub minimum_required_version: Version,
    pub type_aliases: HashMap<EcoString, TypeAliasConstructor>,
    pub documentation: Vec<EcoString>,
    /// Wether there's any echo in the module.
    pub contains_echo: bool,
    pub references: References,
}

impl ModuleInterface {
    pub fn contains_todo(&self) -> bool {
        self.warnings.iter().any(|warning| warning.is_todo())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct References {
    pub imported_modules: HashSet<EcoString>,
    pub value_references: ReferenceMap,
    pub type_references: ReferenceMap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opaque {
    Opaque,
    NotOpaque,
}

/// Information on the constructors of a custom type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeVariantConstructors {
    /// The id of the generic type variables of the generic version of the type that these
    /// constructors belong to.
    /// For example, if we have this type:
    ///
    /// ```gleam
    /// pub type Option(a) {
    ///   Some(a)
    ///   None
    /// }
    /// ```
    ///
    /// and `a` is a Generic type variable with id 1, then this field will be `[1]`.
    ///
    pub type_parameters_ids: Vec<u64>,
    pub opaque: Opaque,
    pub variants: Vec<TypeValueConstructor>,
}

impl TypeVariantConstructors {
    pub(crate) fn new(
        variants: Vec<TypeValueConstructor>,
        type_parameters: &[&EcoString],
        opaque: Opaque,
        hydrator: Hydrator,
    ) -> TypeVariantConstructors {
        let named_types = hydrator.named_type_variables();
        let type_parameters = type_parameters
            .iter()
            .map(|&p| {
                let t = named_types
                    .get(p)
                    .expect("Type parameter not found in hydrator");
                let error = "Hydrator must not store non generic types here";
                match t.type_.as_ref() {
                    Type::Var { type_ } => match type_.borrow().deref() {
                        TypeVar::Generic { id } => *id,
                        _ => panic!("{}", error),
                    },
                    _ => panic!("{}", error),
                }
            })
            .collect_vec();
        Self {
            type_parameters_ids: type_parameters,
            variants,
            opaque,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeValueConstructor {
    pub name: EcoString,
    pub parameters: Vec<TypeValueConstructorField>,
    pub documentation: Option<EcoString>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeValueConstructorField {
    /// This type of this parameter
    pub type_: Arc<Type>,
    pub label: Option<EcoString>,
}

impl ModuleInterface {
    pub fn get_public_value(&self, name: &str) -> Option<&ValueConstructor> {
        let value = self.values.get(name)?;
        if value.publicity.is_importable() {
            Some(value)
        } else {
            None
        }
    }

    pub fn get_public_type(&self, name: &str) -> Option<&TypeConstructor> {
        let type_ = self.types.get(name)?;
        if type_.publicity.is_importable() {
            Some(type_)
        } else {
            None
        }
    }

    pub fn get_main_function(&self, target: Target) -> Result<ModuleFunction, crate::Error> {
        // Module must have a value with the name "main"
        let Some(value) = self.values.get(&EcoString::from("main")) else {
            return Err(crate::Error::ModuleDoesNotHaveMainFunction {
                module: self.name.clone(),
                origin: self.origin,
            });
        };

        assert_suitable_main_function(value, &self.name, self.origin, target)?;

        Ok(ModuleFunction {
            package: self.package.clone(),
        })
    }

    pub fn public_value_names(&self) -> Vec<EcoString> {
        self.values
            .iter()
            .filter(|(_, v)| v.publicity.is_importable())
            .map(|(k, _)| k)
            .cloned()
            .collect_vec()
    }

    pub fn public_type_names(&self) -> Vec<EcoString> {
        self.types
            .iter()
            .filter(|(_, v)| v.publicity.is_importable())
            .map(|(k, _)| k)
            .cloned()
            .collect_vec()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatternConstructor {
    pub name: EcoString,
    pub field_map: Option<FieldMap>,
    pub documentation: Option<EcoString>,
    pub module: EcoString,
    pub location: SrcSpan,
    pub constructor_index: u16,
}

impl PatternConstructor {
    pub fn definition_location(&self) -> Option<DefinitionLocation> {
        Some(DefinitionLocation {
            module: Some(self.module.clone()),
            span: self.location,
        })
    }

    pub fn get_documentation(&self) -> Option<&str> {
        self.documentation.as_deref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum TypeVar {
    /// Unbound is an unbound variable. It is one specific type but we don't
    /// know what yet in the inference process. It has a unique id which can be used to
    /// identify if two unbound variable Rust values are the same Gleam type variable
    /// instance or not.
    ///
    Unbound { id: u64 },
    /// Link is type variable where it was an unbound variable but we worked out
    /// that it is some other type and now we point to that one.
    ///
    Link { type_: Arc<Type> },
    /// A Generic variable stands in for any possible type and cannot be
    /// specialised to any one type
    ///
    /// # Example
    ///
    /// ```gleam
    /// type Cat(a) {
    ///   Cat(name: a)
    /// }
    /// // a is TypeVar::Generic
    /// ```
    ///
    Generic { id: u64 },
}

impl TypeVar {
    pub fn is_unbound(&self) -> bool {
        match self {
            Self::Unbound { .. } => true,
            Self::Link { .. } | Self::Generic { .. } => false,
        }
    }

    pub fn is_variable(&self) -> bool {
        match self {
            Self::Unbound { .. } | Self::Generic { .. } => true,
            Self::Link { type_ } => type_.is_variable(),
        }
    }

    pub fn return_type(&self) -> Option<Arc<Type>> {
        match self {
            Self::Link { type_ } => type_.return_type(),
            Self::Unbound { .. } | Self::Generic { .. } => None,
        }
    }

    pub fn tuple_types(&self) -> Option<Vec<Arc<Type>>> {
        match self {
            Self::Link { type_ } => type_.tuple_types(),
            Self::Unbound { .. } | Self::Generic { .. } => None,
        }
    }

    pub fn constructor_types(&self) -> Option<Vec<Arc<Type>>> {
        match self {
            Self::Link { type_ } => type_.constructor_types(),
            Self::Unbound { .. } | Self::Generic { .. } => None,
        }
    }

    pub fn custom_type_inferred_variant(&self) -> Option<u16> {
        match self {
            Self::Link { type_ } => type_.custom_type_inferred_variant(),
            Self::Unbound { .. } | Self::Generic { .. } => None,
        }
    }

    pub fn is_result(&self) -> bool {
        match self {
            Self::Link { type_ } => type_.is_result(),
            Self::Unbound { .. } | Self::Generic { .. } => false,
        }
    }

    pub fn result_ok_type(&self) -> Option<Arc<Type>> {
        match self {
            TypeVar::Link { type_ } => type_.result_ok_type(),
            TypeVar::Unbound { .. } | TypeVar::Generic { .. } => None,
        }
    }

    pub fn result_types(&self) -> Option<(Arc<Type>, Arc<Type>)> {
        match self {
            TypeVar::Link { type_ } => type_.result_types(),
            TypeVar::Unbound { .. } | TypeVar::Generic { .. } => None,
        }
    }

    pub fn fn_types(&self) -> Option<(Vec<Arc<Type>>, Arc<Type>)> {
        match self {
            Self::Link { type_ } => type_.fn_types(),
            Self::Unbound { .. } | Self::Generic { .. } => None,
        }
    }

    #[must_use]
    pub fn is_fun(&self) -> bool {
        match self {
            TypeVar::Link { type_ } => type_.is_fun(),
            TypeVar::Unbound { .. } | TypeVar::Generic { .. } => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Self::Link { type_ } => type_.is_nil(),
            Self::Unbound { .. } | Self::Generic { .. } => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::Link { type_ } => type_.is_bool(),
            Self::Unbound { .. } | Self::Generic { .. } => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Self::Link { type_ } => type_.is_int(),
            Self::Unbound { .. } | Self::Generic { .. } => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::Link { type_ } => type_.is_float(),
            Self::Unbound { .. } | Self::Generic { .. } => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::Link { type_ } => type_.is_string(),
            Self::Unbound { .. } | Self::Generic { .. } => false,
        }
    }

    pub fn named_type_name(&self) -> Option<(EcoString, EcoString)> {
        match self {
            Self::Link { type_ } => type_.named_type_name(),
            Self::Unbound { .. } | Self::Generic { .. } => None,
        }
    }

    pub fn named_type_information(&self) -> Option<(EcoString, EcoString, Vec<Arc<Type>>)> {
        match self {
            Self::Link { type_ } => type_.named_type_information(),
            Self::Unbound { .. } | Self::Generic { .. } => None,
        }
    }

    pub fn set_custom_type_variant(&mut self, index: u16) {
        match self {
            Self::Link { type_ } => Arc::make_mut(type_).set_custom_type_variant(index),
            Self::Unbound { .. } | Self::Generic { .. } => {}
        }
    }

    pub fn generalise_custom_type_variant(&mut self) {
        match self {
            Self::Link { type_ } => Arc::make_mut(type_).generalise_custom_type_variant(),
            Self::Unbound { .. } | Self::Generic { .. } => {}
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructor {
    pub publicity: Publicity,
    pub origin: SrcSpan,
    pub module: EcoString,
    pub parameters: Vec<Arc<Type>>,
    pub type_: Arc<Type>,
    pub deprecation: Deprecation,
    pub documentation: Option<EcoString>,
}

impl TypeConstructor {
    pub(crate) fn with_location(mut self, location: SrcSpan) -> Self {
        self.origin = location;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueConstructor {
    pub publicity: Publicity,
    pub deprecation: Deprecation,
    pub variant: ValueConstructorVariant,
    pub type_: Arc<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Deprecation {
    NotDeprecated,
    Deprecated { message: EcoString },
}

impl Deprecation {
    /// Returns `true` if the deprecation is [`Deprecated`].
    ///
    /// [`Deprecated`]: Deprecation::Deprecated
    #[must_use]
    pub fn is_deprecated(&self) -> bool {
        matches!(self, Self::Deprecated { .. })
    }
}

impl Default for Deprecation {
    fn default() -> Self {
        Self::NotDeprecated
    }
}

impl ValueConstructor {
    pub fn local_variable(location: SrcSpan, origin: VariableOrigin, type_: Arc<Type>) -> Self {
        Self {
            publicity: Publicity::Private,
            deprecation: Deprecation::NotDeprecated,
            variant: ValueConstructorVariant::LocalVariable { location, origin },
            type_,
        }
    }

    pub fn is_local_variable(&self) -> bool {
        self.variant.is_local_variable()
    }

    pub fn definition_location(&self) -> DefinitionLocation {
        match &self.variant {
            ValueConstructorVariant::Record {
                module, location, ..
            }
            | ValueConstructorVariant::ModuleConstant {
                location, module, ..
            }
            | ValueConstructorVariant::ModuleFn {
                location, module, ..
            } => DefinitionLocation {
                module: Some(module.clone()),
                span: *location,
            },

            ValueConstructorVariant::LocalConstant { literal } => DefinitionLocation {
                module: None,
                span: literal.location(),
            },

            ValueConstructorVariant::LocalVariable { location, .. } => DefinitionLocation {
                module: None,
                span: *location,
            },
        }
    }

    pub(crate) fn get_documentation(&self) -> Option<&str> {
        match &self.variant {
            ValueConstructorVariant::LocalConstant { .. }
            | ValueConstructorVariant::LocalVariable { .. } => Some("A locally defined variable."),

            ValueConstructorVariant::ModuleFn { documentation, .. }
            | ValueConstructorVariant::Record { documentation, .. }
            | ValueConstructorVariant::ModuleConstant { documentation, .. } => {
                Some(documentation.as_ref()?.as_str())
            }
        }
    }

    /// Returns the purity of this value constructor if it is called as a function.
    /// Referencing a value constructor by itself is always pure, but calling is as a
    /// function might not be.
    pub fn called_function_purity(&self) -> Purity {
        match &self.variant {
            // If we call a module constant or local variable as a function, we
            // no longer have enough information to determine its purity. For
            // example:
            //
            // ```gleam
            // const function1 = io.println
            // const function2 = function.identity
            //
            // pub fn main() {
            //   function1("Hello")
            //   function2("Hello")
            // }
            // ```
            //
            // At this point, we don't have any information about the purity of
            // the `function1` and `function2` functions, and must return
            // `Purity::Unknown`. See the documentation for the `Purity` type
            // for more information on why this is the case.
            ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::LocalConstant { .. } => Purity::Unknown,

            // Constructing records is always pure
            ValueConstructorVariant::Record { .. } => Purity::Pure,

            ValueConstructorVariant::ModuleFn { purity, .. } => *purity,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAliasConstructor {
    pub publicity: Publicity,
    pub module: EcoString,
    pub type_: Arc<Type>,
    pub arity: usize,
    pub deprecation: Deprecation,
    pub documentation: Option<EcoString>,
    pub origin: SrcSpan,
}

impl ValueConstructor {
    pub fn field_map(&self) -> Option<&FieldMap> {
        match &self.variant {
            ValueConstructorVariant::ModuleFn { field_map, .. }
            | ValueConstructorVariant::Record { field_map, .. } => field_map.as_ref(),
            _ => None,
        }
    }
}

pub type TypedCallArg = CallArg<TypedExpr>;

fn assert_no_labelled_arguments<A>(args: &[CallArg<A>]) -> Result<(), Error> {
    for arg in args {
        if let Some(label) = &arg.label {
            return Err(Error::UnexpectedLabelledArg {
                location: arg.location,
                label: label.clone(),
            });
        }
    }
    Ok(())
}

/// This function makes sure that the type variable being unified
/// doesn't occur within the type it is being unified with. This
/// prevents the algorithm from inferring recursive types, which
/// could cause naively-implemented type checking to diverge.
/// While traversing the type tree.
///
fn unify_unbound_type(type_: Arc<Type>, own_id: u64) -> Result<(), UnifyError> {
    if let Type::Var { type_ } = type_.deref() {
        let new_value = match type_.borrow().deref() {
            TypeVar::Link { type_, .. } => return unify_unbound_type(type_.clone(), own_id),

            TypeVar::Unbound { id } => {
                if id == &own_id {
                    return Err(UnifyError::RecursiveType);
                } else {
                    Some(TypeVar::Unbound { id: *id })
                }
            }

            TypeVar::Generic { .. } => return Ok(()),
        };

        if let Some(t) = new_value {
            *type_.borrow_mut() = t;
        }
        return Ok(());
    }

    match type_.deref() {
        Type::Named { args, .. } => {
            for arg in args {
                unify_unbound_type(arg.clone(), own_id)?
            }
            Ok(())
        }

        Type::Fn { args, return_ } => {
            for arg in args {
                unify_unbound_type(arg.clone(), own_id)?;
            }
            unify_unbound_type(return_.clone(), own_id)
        }

        Type::Tuple { elements, .. } => {
            for element in elements {
                unify_unbound_type(element.clone(), own_id)?
            }
            Ok(())
        }

        Type::Var { .. } => unreachable!(),
    }
}

fn match_fun_type(
    type_: Arc<Type>,
    arity: usize,
    environment: &mut Environment<'_>,
) -> Result<(Vec<Arc<Type>>, Arc<Type>), MatchFunTypeError> {
    if let Type::Var { type_ } = type_.deref() {
        let new_value = match type_.borrow().deref() {
            TypeVar::Link { type_, .. } => {
                return match_fun_type(type_.clone(), arity, environment);
            }

            TypeVar::Unbound { .. } => {
                let args: Vec<_> = (0..arity).map(|_| environment.new_unbound_var()).collect();
                let return_ = environment.new_unbound_var();
                Some((args, return_))
            }

            TypeVar::Generic { .. } => None,
        };

        if let Some((args, return_)) = new_value {
            *type_.borrow_mut() = TypeVar::Link {
                type_: fn_(args.clone(), return_.clone()),
            };
            return Ok((args, return_));
        }
    }

    if let Type::Fn { args, return_ } = type_.deref() {
        return if args.len() != arity {
            Err(MatchFunTypeError::IncorrectArity {
                expected: args.len(),
                given: arity,
                args: args.clone(),
                return_type: return_.clone(),
            })
        } else {
            Ok((args.clone(), return_.clone()))
        };
    }

    Err(MatchFunTypeError::NotFn { type_ })
}

pub fn generalise(t: Arc<Type>) -> Arc<Type> {
    match t.deref() {
        Type::Var { type_ } => match type_.borrow().deref() {
            TypeVar::Unbound { id } => generic_var(*id),
            TypeVar::Link { type_ } => generalise(type_.clone()),
            TypeVar::Generic { .. } => Arc::new(Type::Var {
                type_: type_.clone(),
            }),
        },

        Type::Named {
            publicity,
            module,
            package,
            name,
            args,
            inferred_variant: _,
        } => {
            let args = args.iter().map(|type_| generalise(type_.clone())).collect();
            Arc::new(Type::Named {
                publicity: *publicity,
                module: module.clone(),
                package: package.clone(),
                name: name.clone(),
                args,
                inferred_variant: None,
            })
        }

        Type::Fn { args, return_ } => fn_(
            args.iter().map(|type_| generalise(type_.clone())).collect(),
            generalise(return_.clone()),
        ),

        Type::Tuple { elements } => tuple(
            elements
                .iter()
                .map(|type_| generalise(type_.clone()))
                .collect(),
        ),
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FieldAccessUsage {
    /// Used as `thing.field()`
    MethodCall,
    /// Used internally when trying to access a field when performing record updates.
    /// The `infer_record_update` function uses this to determine which fields exist
    /// on a certain record to know whether a certain record update updates correct fields.
    /// Without this distinction, we get confusing error messages in certain cases.
    ///
    RecordUpdate,
    /// Used as `thing.field`
    Other,
}

/// Verify that a value is suitable to be used as a main function.
fn assert_suitable_main_function(
    value: &ValueConstructor,
    module_name: &EcoString,
    origin: Origin,
    target: Target,
) -> Result<(), crate::Error> {
    // The value must be a module function
    let ValueConstructorVariant::ModuleFn {
        arity,
        implementations,
        ..
    } = &value.variant
    else {
        return Err(crate::Error::ModuleDoesNotHaveMainFunction {
            module: module_name.clone(),
            origin,
        });
    };

    // The target must be supported
    if !implementations.supports(target) {
        return Err(crate::Error::MainFunctionDoesNotSupportTarget {
            module: module_name.clone(),
            target,
        });
    }

    // The function must be zero arity
    if *arity != 0 {
        return Err(crate::Error::MainFunctionHasWrongArity {
            module: module_name.clone(),
            arity: *arity,
        });
    }

    // The function must be public, or trying to run it would result in a
    // runtime crash
    if !value.publicity.is_importable() {
        return Err(crate::Error::MainFunctionIsPrivate {
            module: module_name.clone(),
        });
    }

    Ok(())
}
