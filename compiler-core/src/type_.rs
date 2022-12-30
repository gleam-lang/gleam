pub(crate) mod environment;
pub(crate) mod error;
pub(crate) mod expression;
pub(crate) mod fields;
pub(crate) mod hydrator;
pub(crate) mod pattern;
pub(crate) mod pipe;
pub(crate) mod prelude;
pub mod pretty;
#[cfg(test)]
mod tests;

pub use environment::*;
pub use error::{Error, UnifyErrorSituation, Warning};
pub(crate) use expression::ExprTyper;
pub use fields::FieldMap;
pub use prelude::*;
use smol_str::SmolStr;

use crate::{
    ast::{
        ArgNames, BitStringSegment, CallArg, Constant, DefinitionLocation, Pattern, SrcSpan,
        TypedConstant, TypedExpr, TypedPattern, TypedPatternBitStringSegment, TypedRecordUpdateArg,
        UntypedMultiPattern, UntypedPattern, UntypedRecordUpdateArg,
    },
    bit_string,
    build::Origin,
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

use serde::{Deserialize, Serialize};

pub trait HasType {
    fn type_(&self) -> Arc<Type>;
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Type {
    /// A nominal (named) type such as `Int`, `Float`, or a programmer defined
    /// custom type such as `Person`. The type can take other types as
    /// arguments (aka "generics" or "parametric polymorphism").
    ///
    /// If the type is defined in the Gleam prelude the `module` field will be
    /// empty, otherwise it will contain the name of the module that
    /// defines the type.
    ///
    App {
        public: bool,
        module: SmolStr,
        name: SmolStr,
        args: Vec<Arc<Type>>,
    },

    /// The type of a function. It takes arguments and returns a value.
    ///
    Fn {
        args: Vec<Arc<Type>>,
        retrn: Arc<Type>,
    },

    /// A type variable. See the contained `TypeVar` enum for more information.
    ///
    Var { type_: Arc<RefCell<TypeVar>> },

    /// A tuple is an ordered collection of 0 or more values, each of which
    /// can have a different type, so the `tuple` type is the sum of all the
    /// contained types.
    ///
    Tuple { elems: Vec<Arc<Type>> },
}

impl Type {
    pub fn is_result_constructor(&self) -> bool {
        match self {
            Type::Fn { retrn, .. } => retrn.is_result(),
            _ => false,
        }
    }

    pub fn is_result(&self) -> bool {
        matches!(self, Self::App { name, module, .. } if "Result" == name && module.is_empty())
    }

    pub fn is_unbound(&self) -> bool {
        matches!(self, Self::Var { type_: typ } if typ.borrow().is_unbound())
    }

    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Var { type_: typ } if typ.borrow().is_variable())
    }

    pub fn return_type(&self) -> Option<Arc<Self>> {
        match self {
            Self::Fn { retrn, .. } => Some(retrn.clone()),
            _ => None,
        }
    }

    pub fn fn_types(&self) -> Option<(Vec<Arc<Self>>, Arc<Self>)> {
        match self {
            Self::Fn { args, retrn, .. } => Some((args.clone(), retrn.clone())),
            _ => None,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "Nil" == name && module.is_empty() => true,
            Self::Var { type_ } => type_.borrow().is_nil(),
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "Bool" == name && module.is_empty() => true,
            Self::Var { type_ } => type_.borrow().is_bool(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "Int" == name && module.is_empty() => true,
            Self::Var { type_ } => type_.borrow().is_int(),
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "Float" == name && module.is_empty() => true,
            Self::Var { type_ } => type_.borrow().is_float(),
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "String" == name && module.is_empty() => true,
            Self::Var { type_ } => type_.borrow().is_string(),
            _ => false,
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
        public: bool,
        module: &str,
        name: &str,
        arity: usize,
        environment: &mut Environment<'_>,
    ) -> Option<Vec<Arc<Self>>> {
        match self {
            Self::App {
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

            Self::Var { type_: typ } => {
                let args: Vec<_> = match typ.borrow().deref() {
                    TypeVar::Link { type_: typ } => {
                        return typ.get_app_args(public, module, name, arity, environment);
                    }

                    TypeVar::Unbound { .. } => {
                        (0..arity).map(|_| environment.new_unbound_var()).collect()
                    }

                    TypeVar::Generic { .. } => return None,
                };

                // We are an unbound type variable! So convert us to a type link
                // to the desired type.
                *typ.borrow_mut() = TypeVar::Link {
                    type_: Arc::new(Self::App {
                        name: name.into(),
                        module: module.into(),
                        args: args.clone(),
                        public,
                    }),
                };
                Some(args)
            }

            _ => None,
        }
    }

    pub fn find_private_type(&self) -> Option<Self> {
        match self {
            Self::App { public: false, .. } => Some(self.clone()),

            Self::App { args, .. } => args.iter().find_map(|t| t.find_private_type()),

            Self::Tuple { elems, .. } => elems.iter().find_map(|t| t.find_private_type()),

            Self::Fn { retrn, args, .. } => retrn
                .find_private_type()
                .or_else(|| args.iter().find_map(|t| t.find_private_type())),

            Self::Var { type_: typ, .. } => match typ.borrow().deref() {
                TypeVar::Unbound { .. } => None,

                TypeVar::Generic { .. } => None,

                TypeVar::Link { type_: typ, .. } => typ.find_private_type(),
            },
        }
    }

    pub fn fn_arity(&self) -> Option<usize> {
        match self {
            Self::Fn { args, .. } => Some(args.len()),
            _ => None,
        }
    }
}

pub fn collapse_links(t: Arc<Type>) -> Arc<Type> {
    if let Type::Var { type_: typ } = t.deref() {
        if let TypeVar::Link { type_: typ } = typ.borrow().deref() {
            return typ.clone();
        }
    }
    t
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AccessorsMap {
    pub public: bool,
    pub type_: Arc<Type>,
    pub accessors: HashMap<SmolStr, RecordAccessor>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordAccessor {
    // TODO: smaller int. Doesn't need to be this big
    pub index: u64,
    pub label: SmolStr,
    pub type_: Arc<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum ValueConstructorVariant {
    /// A locally defined variable or function parameter
    LocalVariable { location: SrcSpan },

    /// A module constant
    ModuleConstant {
        location: SrcSpan,
        module: SmolStr,
        literal: Constant<Arc<Type>, SmolStr>,
    },

    /// A function belonging to the module
    ModuleFn {
        name: SmolStr,
        field_map: Option<FieldMap>,
        module: SmolStr,
        arity: usize,
        location: SrcSpan,
    },

    /// A constructor for a custom type
    Record {
        name: SmolStr,
        arity: u16,
        field_map: Option<FieldMap>,
        location: SrcSpan,
        module: SmolStr,
        constructors_count: u16,
    },
}

impl ValueConstructorVariant {
    fn to_module_value_constructor(
        &self,
        type_: Arc<Type>,
        module_name: &SmolStr,
        function_name: &SmolStr,
    ) -> ModuleValueConstructor {
        match self {
            Self::Record {
                name,
                arity,
                field_map,
                location,
                ..
            } => ModuleValueConstructor::Record {
                name: name.clone(),
                field_map: field_map.clone(),
                arity: *arity,
                type_,
                location: *location,
            },

            // TODO: remove this clone with an rc clone
            Self::ModuleConstant {
                literal, location, ..
            } => ModuleValueConstructor::Constant {
                literal: literal.clone(),
                location: *location,
            },

            Self::LocalVariable { location, .. } => ModuleValueConstructor::Fn {
                name: function_name.clone(),
                module: module_name.clone(),
                location: *location,
            },

            Self::ModuleFn {
                name,
                module,
                location,
                ..
            } => ModuleValueConstructor::Fn {
                name: name.clone(),
                module: module.clone(),
                location: *location,
            },
        }
    }

    pub fn definition_location(&self) -> SrcSpan {
        match self {
            ValueConstructorVariant::LocalVariable { location }
            | ValueConstructorVariant::ModuleConstant { location, .. }
            | ValueConstructorVariant::ModuleFn { location, .. }
            | ValueConstructorVariant::Record { location, .. } => *location,
        }
    }

    /// Returns `true` if the variant is [`LocalVariable`].
    pub fn is_local_variable(&self) -> bool {
        matches!(self, Self::LocalVariable { .. })
    }

    /// Returns `true` if the value constructor variant is [`ModuleFn`].
    ///
    /// [`ModuleFn`]: ValueConstructorVariant::ModuleFn
    #[must_use]
    pub fn is_module_fn(&self) -> bool {
        matches!(self, Self::ModuleFn { .. })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleValueConstructor {
    Record {
        name: SmolStr,
        arity: u16,
        type_: Arc<Type>,
        field_map: Option<FieldMap>,
        location: SrcSpan,
    },

    Fn {
        location: SrcSpan,
        /// The name of the module and the function
        /// Typically this will be the module that this constructor belongs to
        /// and the name that was used for the function. However it could also
        /// point to some other module and function when this is an `external fn`.
        ///
        /// This function has module "themodule" and name "wibble"
        ///     pub fn wibble() { Nil }
        ///
        /// This function has module "other" and name "whoop"
        ///     pub external fn wibble() -> Nil =
        ///       "other" "whoop"
        ///
        module: SmolStr,
        name: SmolStr,
    },

    Constant {
        literal: TypedConstant,
        location: SrcSpan,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub name: SmolStr,
    pub origin: Origin,
    pub package: SmolStr,
    pub types: HashMap<SmolStr, TypeConstructor>,
    pub types_constructors: HashMap<SmolStr, Vec<SmolStr>>,
    pub values: HashMap<SmolStr, ValueConstructor>,
    pub accessors: HashMap<SmolStr, AccessorsMap>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternConstructor {
    Record {
        name: SmolStr,
        field_map: Option<FieldMap>,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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
        matches!(self, Self::Unbound { .. })
    }

    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Unbound { .. } | Self::Generic { .. })
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Self::Link { type_ } => type_.is_nil(),
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::Link { type_ } => type_.is_bool(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Self::Link { type_ } => type_.is_int(),
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::Link { type_ } => type_.is_float(),
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::Link { type_ } => type_.is_string(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructor {
    pub public: bool,
    pub origin: SrcSpan,
    pub module: SmolStr,
    pub parameters: Vec<Arc<Type>>,
    pub typ: Arc<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct ValueConstructor {
    pub public: bool,
    pub variant: ValueConstructorVariant,
    pub type_: Arc<Type>,
}

impl ValueConstructor {
    pub fn is_local_variable(&self) -> bool {
        self.variant.is_local_variable()
    }

    pub fn definition_location(&self) -> DefinitionLocation<'_> {
        match &self.variant {
            ValueConstructorVariant::Record {
                module, location, ..
            }
            | ValueConstructorVariant::ModuleConstant {
                location, module, ..
            } => DefinitionLocation {
                module: Some(module.as_str()),
                span: *location,
            },

            ValueConstructorVariant::ModuleFn { location, .. }
            | ValueConstructorVariant::LocalVariable { location } => DefinitionLocation {
                module: None,
                span: *location,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAliasConstructor {
    pub public: bool,
    pub module: SmolStr,
    pub type_: Type,
    pub arity: usize,
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
fn unify_unbound_type(typ: Arc<Type>, own_id: u64) -> Result<(), UnifyError> {
    if let Type::Var { type_: typ } = typ.deref() {
        let new_value = match typ.borrow().deref() {
            TypeVar::Link { type_: typ, .. } => return unify_unbound_type(typ.clone(), own_id),

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
            *typ.borrow_mut() = t;
        }
        return Ok(());
    }

    match typ.deref() {
        Type::App { args, .. } => {
            for arg in args {
                unify_unbound_type(arg.clone(), own_id)?
            }
            Ok(())
        }

        Type::Fn { args, retrn } => {
            for arg in args {
                unify_unbound_type(arg.clone(), own_id)?;
            }
            unify_unbound_type(retrn.clone(), own_id)
        }

        Type::Tuple { elems, .. } => {
            for elem in elems {
                unify_unbound_type(elem.clone(), own_id)?
            }
            Ok(())
        }

        Type::Var { .. } => unreachable!(),
    }
}

fn match_fun_type(
    typ: Arc<Type>,
    arity: usize,
    environment: &mut Environment<'_>,
) -> Result<(Vec<Arc<Type>>, Arc<Type>), MatchFunTypeError> {
    if let Type::Var { type_: typ } = typ.deref() {
        let new_value = match typ.borrow().deref() {
            TypeVar::Link { type_: typ, .. } => {
                return match_fun_type(typ.clone(), arity, environment);
            }

            TypeVar::Unbound { .. } => {
                let args: Vec<_> = (0..arity).map(|_| environment.new_unbound_var()).collect();
                let retrn = environment.new_unbound_var();
                Some((args, retrn))
            }

            TypeVar::Generic { .. } => None,
        };

        if let Some((args, retrn)) = new_value {
            *typ.borrow_mut() = TypeVar::Link {
                type_: fn_(args.clone(), retrn.clone()),
            };
            return Ok((args, retrn));
        }
    }

    if let Type::Fn { args, retrn } = typ.deref() {
        return if args.len() != arity {
            Err(MatchFunTypeError::IncorrectArity {
                expected: args.len(),
                given: arity,
            })
        } else {
            Ok((args.clone(), retrn.clone()))
        };
    }

    Err(MatchFunTypeError::NotFn { typ })
}

pub fn generalise(t: Arc<Type>) -> Arc<Type> {
    match t.deref() {
        Type::Var { type_: typ } => match typ.borrow().deref() {
            TypeVar::Unbound { id } => generic_var(*id),
            TypeVar::Link { type_: typ } => generalise(typ.clone()),
            TypeVar::Generic { .. } => Arc::new(Type::Var { type_: typ.clone() }),
        },

        Type::App {
            public,
            module,
            name,
            args,
        } => {
            let args = args.iter().map(|t| generalise(t.clone())).collect();
            Arc::new(Type::App {
                public: *public,
                module: module.clone(),
                name: name.clone(),
                args,
            })
        }

        Type::Fn { args, retrn } => fn_(
            args.iter().map(|t| generalise(t.clone())).collect(),
            generalise(retrn.clone()),
        ),

        Type::Tuple { elems } => tuple(elems.iter().map(|t| generalise(t.clone())).collect()),
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FieldAccessUsage {
    /// Used as `thing.field()`
    MethodCall,
    /// Used as `thing.field`
    Other,
}
