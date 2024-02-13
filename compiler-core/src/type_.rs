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
pub mod tests;

use ecow::EcoString;
pub use environment::*;
pub use error::{Error, UnifyErrorSituation, Warning};
pub(crate) use expression::ExprTyper;
pub use fields::FieldMap;
pub use prelude::*;
use serde::Serialize;

use crate::{
    ast::{
        ArgNames, BitArraySegment, CallArg, Constant, DefinitionLocation, Pattern, SrcSpan,
        TypedConstant, TypedExpr, TypedPattern, TypedPatternBitArraySegment, TypedRecordUpdateArg,
        UntypedMultiPattern, UntypedPattern, UntypedRecordUpdateArg,
    },
    bit_array,
    build::{Origin, Target},
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

use self::expression::Implementations;

pub trait HasType {
    fn type_(&self) -> Arc<Type>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
        public: bool,
        package: EcoString,
        module: EcoString,
        name: EcoString,
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
        matches!(self, Self::Named { name, module, .. } if "Result" == name && is_prelude_module(module))
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

    /// Get the args for the type if the type is a specific `Type::App`.
    /// Returns None if the type is not a `Type::App` or is an incorrect `Type:App`
    ///
    /// This function is currently only used for finding the `List` type.
    ///
    // TODO: specialise this to just List.
    pub fn get_app_args(
        &self,
        public: bool,
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

            Self::Var { type_: typ } => {
                let args: Vec<_> = match typ.borrow().deref() {
                    TypeVar::Link { type_: typ } => {
                        return typ.get_app_args(public, package, module, name, arity, environment);
                    }

                    TypeVar::Unbound { .. } => {
                        (0..arity).map(|_| environment.new_unbound_var()).collect()
                    }

                    TypeVar::Generic { .. } => return None,
                };

                // We are an unbound type variable! So convert us to a type link
                // to the desired type.
                *typ.borrow_mut() = TypeVar::Link {
                    type_: Arc::new(Self::Named {
                        name: name.into(),
                        package: package.into(),
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
            Self::Named { public: false, .. } => Some(self.clone()),

            Self::Named { args, .. } => args.iter().find_map(|t| t.find_private_type()),

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
            return collapse_links(typ.clone());
        }
    }
    t
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AccessorsMap {
    pub public: bool,
    pub type_: Arc<Type>,
    pub accessors: HashMap<EcoString, RecordAccessor>,
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
    LocalVariable { location: SrcSpan },

    /// A module constant
    ModuleConstant {
        documentation: Option<EcoString>,
        location: SrcSpan,
        module: EcoString,
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
    },

    /// A constructor for a custom type
    Record {
        name: EcoString,
        arity: u16,
        field_map: Option<FieldMap>,
        location: SrcSpan,
        module: EcoString,
        constructors_count: u16,
        constructor_index: u16,
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
                documentation: None,
                location: *location,
            },

            Self::ModuleFn {
                name,
                module,
                location,
                documentation,
                ..
            } => ModuleValueConstructor::Fn {
                name: name.clone(),
                module: module.clone(),
                documentation: documentation.clone(),
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
            ValueConstructorVariant::LocalConstant { literal } => literal.location(),
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

    pub fn implementations(&self) -> Implementations {
        match self {
            ValueConstructorVariant::Record { .. }
            | ValueConstructorVariant::LocalConstant { .. }
            | ValueConstructorVariant::LocalVariable { .. } => Implementations {
                gleam: true,
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
        /// Typically this will be the module that this constructor belongs to
        /// and the name that was used for the function. However it could also
        /// point to some other module and function when this is an `external`
        /// function.
        ///
        /// This function has module "themodule" and name "wibble"
        ///     pub fn wibble() { Nil }
        ///
        /// This function has module "other" and name "whoop"
        ///     @external(erlang, "other", "whoop")
        ///     pub fn wibble() -> Nil
        ///
        module: EcoString,
        name: EcoString,
        documentation: Option<EcoString>,
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
    pub unused_imports: Vec<SrcSpan>,
    pub contains_todo: bool,
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
    pub variants: Vec<TypeValueConstructor>,
}

impl TypeVariantConstructors {
    pub(crate) fn new(
        variants: Vec<TypeValueConstructor>,
        type_parameters: &[EcoString],
        hydrator: Hydrator,
    ) -> TypeVariantConstructors {
        let named_types = hydrator.named_type_variables();
        let type_parameters = type_parameters
            .iter()
            .map(|p| {
                let t = named_types
                    .get(p)
                    .expect("Type parameter not found in hydrator");
                let error = "Hydrator must not store non generic types here";
                match t.type_.as_ref() {
                    Type::Var { type_: typ } => match typ.borrow().deref() {
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
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeValueConstructor {
    pub name: EcoString,
    pub parameters: Vec<TypeValueConstructorField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeValueConstructorField {
    /// This type of this parameter
    pub type_: Arc<Type>,
}

impl ModuleInterface {
    pub fn new(name: EcoString, origin: Origin, package: EcoString) -> Self {
        Self {
            name,
            origin,
            package,
            types: Default::default(),
            types_value_constructors: Default::default(),
            values: Default::default(),
            accessors: Default::default(),
            unused_imports: Default::default(),
            contains_todo: false,
        }
    }

    pub fn get_public_value(&self, name: &str) -> Option<&ValueConstructor> {
        let value = self.values.get(name)?;
        if value.public {
            Some(value)
        } else {
            None
        }
    }

    pub fn get_public_type(&self, name: &str) -> Option<&TypeConstructor> {
        let type_ = self.types.get(name)?;
        if type_.public {
            Some(type_)
        } else {
            None
        }
    }

    pub fn get_main_function(&self, target: Target) -> Result<ModuleFunction, crate::Error> {
        let not_found = || crate::Error::ModuleDoesNotHaveMainFunction {
            module: self.name.clone(),
        };

        // Module must have a value with the name "main"
        let value = self
            .values
            .get(&EcoString::from("main"))
            .ok_or_else(not_found)?;

        assert_suitable_main_function(value, &self.name, target)?;

        Ok(ModuleFunction {
            package: self.package.clone(),
        })
    }

    pub fn public_value_names(&self) -> Vec<EcoString> {
        self.values
            .iter()
            .filter(|(_, v)| v.public)
            .map(|(k, _)| k)
            .cloned()
            .collect_vec()
    }

    pub fn public_type_names(&self) -> Vec<EcoString> {
        self.types
            .iter()
            .filter(|(_, v)| v.public)
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
    pub module: Option<EcoString>,
    pub location: SrcSpan,
    pub constructor_index: u16,
}

impl PatternConstructor {
    pub fn definition_location(&self) -> Option<DefinitionLocation<'_>> {
        Some(DefinitionLocation {
            module: Some(self.module.as_deref()?),
            span: self.location,
        })
    }

    pub fn get_documentation(&self) -> Option<&str> {
        self.documentation.as_deref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

    pub fn named_type_name(&self) -> Option<(EcoString, EcoString)> {
        match self {
            Self::Link { type_ } => type_.named_type_name(),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructor {
    pub public: bool,
    pub origin: SrcSpan,
    pub module: EcoString,
    pub parameters: Vec<Arc<Type>>,
    pub typ: Arc<Type>,
    pub deprecation: Deprecation,
}
impl TypeConstructor {
    pub(crate) fn with_location(mut self, location: SrcSpan) -> Self {
        self.origin = location;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueConstructor {
    pub public: bool,
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

            ValueConstructorVariant::LocalConstant { literal } => DefinitionLocation {
                module: None,
                span: literal.location(),
            },

            ValueConstructorVariant::ModuleFn { location, .. }
            | ValueConstructorVariant::LocalVariable { location } => DefinitionLocation {
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAliasConstructor {
    pub public: bool,
    pub module: EcoString,
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
        Type::Named { args, .. } => {
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

        Type::Named {
            public,
            module,
            package,
            name,
            args,
        } => {
            let args = args.iter().map(|t| generalise(t.clone())).collect();
            Arc::new(Type::Named {
                public: *public,
                module: module.clone(),
                package: package.clone(),
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

/// Verify that a value is suitable to be used as a main function.
fn assert_suitable_main_function(
    value: &ValueConstructor,
    module_name: &EcoString,
    target: Target,
) -> Result<(), crate::Error> {
    let not_found = || crate::Error::ModuleDoesNotHaveMainFunction {
        module: module_name.clone(),
    };

    // The value must be a module function
    let ValueConstructorVariant::ModuleFn {
        arity,
        implementations,
        ..
    } = &value.variant
    else {
        return Err(not_found());
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

    Ok(())
}
