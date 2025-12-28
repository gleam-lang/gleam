use std::{collections::HashMap, ops::Deref};

use ecow::EcoString;
use serde::Serialize;

#[cfg(test)]
mod tests;

use crate::{
    io::ordered_map,
    type_::{
        self, Deprecation, Opaque, Type, TypeConstructor, TypeVar, TypeVariantConstructors,
        ValueConstructorVariant, expression::Implementations,
    },
};

use crate::build::Package;

/// The public interface of a package that gets serialised as a json object.
#[derive(Serialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct PackageInterface {
    name: EcoString,
    version: EcoString,
    /// The Gleam version constraint that the package specifies in its `gleam.toml`.
    gleam_version_constraint: Option<EcoString>,
    /// A map from module name to its interface.
    #[serde(serialize_with = "ordered_map")]
    modules: HashMap<EcoString, ModuleInterface>,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct ModuleInterface {
    /// A vector with the lines composing the module's documentation (that is
    /// every line preceded by a `////`).
    documentation: Vec<EcoString>,
    /// A map from type alias name to its interface.
    #[serde(serialize_with = "ordered_map")]
    type_aliases: HashMap<EcoString, TypeAliasInterface>,
    /// A map from type name to its interface.
    #[serde(serialize_with = "ordered_map")]
    types: HashMap<EcoString, TypeDefinitionInterface>,
    /// A map from constant name to its interface.
    #[serde(serialize_with = "ordered_map")]
    constants: HashMap<EcoString, ConstantInterface>,
    /// A map from function name to its interface.
    #[serde(serialize_with = "ordered_map")]
    functions: HashMap<EcoString, FunctionInterface>,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct TypeDefinitionInterface {
    /// The definition's documentation comment (that is every line preceded by
    /// `///`).
    documentation: Option<EcoString>,
    /// If the definition has a deprecation annotation `@deprecated("...")`
    /// this field will hold the reason of the deprecation.
    deprecation: Option<DeprecationInterface>,
    /// The number of type variables in the type definition.
    /// ```gleam
    /// /// This type has 2 type variables.
    /// type Result(a, b) {
    ///   Ok(a)
    ///   Error(b)
    /// }
    /// ```
    parameters: usize,
    /// A list of the type constructors. If the type is marked as opaque it
    /// won't have any visible constructors.
    constructors: Vec<TypeConstructorInterface>,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct TypeConstructorInterface {
    /// The constructor's documentation comment (that is every line preceded by
    /// `///`).
    documentation: Option<EcoString>,
    /// The name of the type constructor.
    /// ```gleam
    /// pub type Box(a) {
    ///   MyBox(value: a)
    /// //^^^^^ This is the constructor's name
    /// }
    /// ```
    name: EcoString,
    /// A list of the parameters needed by the constructor.
    /// ```gleam
    /// pub type Box(a) {
    ///   MyBox(value: a)
    /// //      ^^^^^^^^ This is the constructor's parameter.
    /// }
    /// ```
    parameters: Vec<ParameterInterface>,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct TypeAliasInterface {
    /// The constructor's documentation comment (that is every line preceded by
    /// `///`).
    documentation: Option<EcoString>,
    /// If the alias has a deprecation annotation `@deprecated("...")`
    /// this field will hold the reason of the deprecation.
    deprecation: Option<DeprecationInterface>,
    /// The number of type variables in the type alias definition.
    /// ```gleam
    /// /// This type alias has 2 type variables.
    /// type Results(a, b) = List(Restul(a, b))
    /// ```
    parameters: usize,
    /// The aliased type.
    /// ```gleam
    /// type Ints = List(Int)
    /// //          ^^^^^^^^^ This is the aliased type in a type alias.
    /// ```
    alias: TypeInterface,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct ConstantInterface {
    /// The constant's documentation comment (that is every line preceded by
    /// `///`).
    documentation: Option<EcoString>,
    /// If the constant has a deprecation annotation `@deprecated("...")`
    /// this field will hold the reason of the deprecation.
    deprecation: Option<DeprecationInterface>,
    implementations: ImplementationsInterface,
    /// The constant's type.
    #[serde(rename = "type")]
    type_: TypeInterface,
}

/// A module's function. This differs from a simple `Fn` type as its arguments
/// can be labelled.
#[derive(Serialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct FunctionInterface {
    /// The function's documentation comment (that is every line preceded by
    /// `///`).
    documentation: Option<EcoString>,
    /// If the constant has a deprecation annotation `@deprecated("...")`
    /// this field will hold the reason of the deprecation.
    deprecation: Option<DeprecationInterface>,
    implementations: ImplementationsInterface,
    parameters: Vec<ParameterInterface>,
    #[serde(rename = "return")]
    return_: TypeInterface,
}

/// Informations about how a value is implemented.
#[derive(Debug, Serialize, Copy, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct ImplementationsInterface {
    /// Set to `true` if the const/function has a pure Gleam implementation
    /// (that is, it never uses external code).
    /// Being pure Gleam means that the function will support all Gleam
    /// targets, even future ones that are not present to this day.
    ///
    /// Consider the following function:
    ///
    /// ```gleam
    /// @external(erlang, "wibble", "wobble")
    /// pub fn a_random_number() -> Int {
    ///   4
    ///   // This is a default implementation.
    /// }
    /// ```
    ///
    /// The implementations for this function will look like this:
    ///
    /// ```json
    /// {
    ///   gleam: true,
    ///   can_run_on_erlang: true,
    ///   can_run_on_javascript: true,
    ///   uses_erlang_externals: true,
    ///   uses_javascript_externals: false,
    /// }
    /// ```
    ///
    /// - `gleam: true` means that the function has a pure Gleam implementation
    ///   and thus it can be used on all Gleam targets with no problems.
    /// - `can_run_on_erlang: false` the function can be called on the Erlang
    ///   target.
    /// - `can_run_on_javascript: true` the function can be called on the JavaScript
    ///   target.
    /// - `uses_erlang_externals: true` means that the function will use Erlang
    ///   external code when compiled to the Erlang target.
    /// - `uses_javascript_externals: false` means that the function won't use
    ///   JavaScript external code when compiled to JavaScript. The function can
    ///   still be used on the JavaScript target since it has a pure Gleam
    ///   implementation.
    gleam: bool,
    /// Set to `true` if the const/function is defined using Erlang external
    /// code. That means that the function will use Erlang code through FFI when
    /// compiled for the Erlang target.
    uses_erlang_externals: bool,
    /// Set to `true` if the const/function is defined using JavaScript external
    /// code. That means that the function will use JavaScript code through FFI
    /// when compiled for the JavaScript target.
    ///
    /// Let's have a look at an example:
    ///
    /// ```gleam
    /// @external(javascript, "wibble", "wobble")
    /// pub fn javascript_only() -> Int
    /// ```
    ///
    /// It's implementations field will look like this:
    ///
    /// ```json
    /// {
    ///   gleam: false,
    ///   can_run_on_erlang: false,
    ///   can_run_on_javascript: true,
    ///   uses_erlang_externals: false,
    ///   uses_javascript_externals: true,
    /// }
    /// ```
    ///
    /// - `gleam: false` means that the function doesn't have a pure Gleam
    ///   implementations. This means that the function is only defined using
    ///   externals and can only be used on some targets.
    /// - `can_run_on_erlang: false` the function cannot be called on the Erlang
    ///   target.
    /// - `can_run_on_javascript: true` the function can be called on the JavaScript
    ///   target.
    /// - `uses_erlang_externals: false` the function is not using external
    ///   Erlang code.
    /// - `uses_javascript_externals: true` the function is using JavaScript
    ///   external code.
    uses_javascript_externals: bool,
    /// Whether the function can be called on the Erlang target, either due to a
    /// pure Gleam implementation or an implementation that uses some Erlang
    /// externals.
    can_run_on_erlang: bool,
    /// Whether the function can be called on the JavaScript target, either due
    /// to a pure Gleam implementation or an implementation that uses some
    /// JavaScript externals.
    can_run_on_javascript: bool,
}

impl ImplementationsInterface {
    fn from_implementations(implementations: &Implementations) -> ImplementationsInterface {
        // It might look a bit silly to just recreate an identical structure with
        // a different name. However, this way we won't inadvertently cause breaking
        // changes if we were to change the names used by the `Implementations` struct
        // that is used by the target tracking algorithm.
        // By doing this we can change the target tracking and package interface
        // separately!
        //
        // This pattern matching makes sure we will remember to handle any change
        // in the `Implementations` struct.
        let Implementations {
            gleam,
            uses_erlang_externals,
            uses_javascript_externals,

            can_run_on_erlang,
            can_run_on_javascript,
        } = implementations;

        ImplementationsInterface {
            gleam: *gleam,
            uses_erlang_externals: *uses_erlang_externals,
            uses_javascript_externals: *uses_javascript_externals,
            can_run_on_erlang: *can_run_on_erlang,
            can_run_on_javascript: *can_run_on_javascript,
        }
    }
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct DeprecationInterface {
    /// The reason for the deprecation.
    message: EcoString,
}

impl DeprecationInterface {
    fn from_deprecation(deprecation: &Deprecation) -> Option<DeprecationInterface> {
        match deprecation {
            Deprecation::NotDeprecated => None,
            Deprecation::Deprecated { message } => Some(DeprecationInterface {
                message: message.clone(),
            }),
        }
    }
}

#[derive(Serialize, Debug)]
#[serde(tag = "kind")]
#[serde(rename_all = "kebab-case")]
pub enum TypeInterface {
    /// A tuple type like `#(Int, Float)`.
    Tuple {
        /// The types composing the tuple.
        elements: Vec<TypeInterface>,
    },
    /// A function type like `fn(Int, String) -> String`.
    Fn {
        parameters: Vec<TypeInterface>,
        #[serde(rename = "return")]
        return_: Box<TypeInterface>,
    },
    /// A type variable.
    /// ```gleam
    /// pub fn wibble(value: a) -> a {}
    /// //                   ^ This is a type variable.
    /// ```
    Variable { id: u64 },
    /// A custom named type.
    /// ```gleam
    /// let value: Bool = True
    ///            ^^^^ This is a named type.
    /// ```
    ///
    /// All prelude types - like Bool, String, etc. - are named types as well.
    /// In that case their package is an empty string `""` and their module
    /// name is the string `"gleam"`.
    ///
    Named {
        name: EcoString,
        /// The package the type is defined in.
        package: EcoString,
        /// The module the type is defined in.
        module: EcoString,
        /// The type parameters that might be needed to define a named type.
        /// ```gleam
        /// let result: Result(Int, e) = Ok(1)
        /// //                 ^^^^^^ The `Result` named type has 2 parameters.
        /// //                        In this case it's the Int type and a type
        /// //                        variable.
        /// ```
        parameters: Vec<TypeInterface>,
    },
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct ParameterInterface {
    /// If the parameter is labelled this will hold the label's name.
    /// ```gleam
    /// pub fn repeat(times n: Int) -> List(Int)
    /// //            ^^^^^ This is the parameter's label.
    /// ```
    label: Option<EcoString>,
    /// The parameter's type.
    /// ```gleam
    /// pub fn repeat(times n: Int) -> List(Int)
    /// //                     ^^^ This is the parameter's type.
    /// ```
    #[serde(rename = "type")]
    type_: TypeInterface,
}

impl PackageInterface {
    pub fn from_package(
        package: &Package,
        cached_modules: &im::HashMap<EcoString, type_::ModuleInterface>,
    ) -> PackageInterface {
        PackageInterface {
            name: package.config.name.clone(),
            version: package.config.version.to_string().into(),
            gleam_version_constraint: package
                .config
                .gleam_version
                .clone()
                .map(|version| EcoString::from(version.hex().to_string())),
            modules: package
                .modules
                .iter()
                .map(|module| &module.ast.type_info)
                .chain(
                    package
                        .cached_module_names
                        .iter()
                        .filter_map(|name| cached_modules.get(name)),
                )
                .filter(|module| !package.config.is_internal_module(module.name.as_str()))
                .map(|module| (module.name.clone(), ModuleInterface::from_interface(module)))
                .collect(),
        }
    }
}

impl ModuleInterface {
    fn from_interface(interface: &type_::ModuleInterface) -> ModuleInterface {
        let mut types = HashMap::new();
        let mut type_aliases = HashMap::new();
        let mut constants = HashMap::new();
        let mut functions = HashMap::new();
        for (name, constructor) in interface.types.iter().filter(|(name, c)| {
            // Aliases are stored separately
            c.publicity.is_public() && !interface.type_aliases.contains_key(*name)
        }) {
            let mut id_map = IdMap::new();

            let TypeConstructor {
                deprecation,
                documentation,
                ..
            } = constructor;

            for typed_parameter in &constructor.parameters {
                id_map.add_type_variable_id(typed_parameter.as_ref());
            }

            let _ = types.insert(
                name.clone(),
                TypeDefinitionInterface {
                    documentation: documentation.clone(),
                    deprecation: DeprecationInterface::from_deprecation(deprecation),
                    parameters: interface
                        .types
                        .get(&name.clone())
                        .map_or(vec![], |t| t.parameters.clone())
                        .len(),
                    constructors: match interface.types_value_constructors.get(&name.clone()) {
                        Some(TypeVariantConstructors {
                            variants,
                            opaque: Opaque::NotOpaque,
                            ..
                        }) => variants
                            .iter()
                            .map(|constructor| TypeConstructorInterface {
                                documentation: constructor.documentation.clone(),
                                name: constructor.name.clone(),
                                parameters: constructor
                                    .parameters
                                    .iter()
                                    .map(|arg| ParameterInterface {
                                        label: arg.label.clone(),
                                        // We share the same id_map between each step so that the
                                        // incremental ids assigned are consisten with each other
                                        type_: from_type_helper(arg.type_.as_ref(), &mut id_map),
                                    })
                                    .collect(),
                            })
                            .collect(),
                        Some(_) | None => Vec::new(),
                    },
                },
            );
        }

        for (name, alias) in interface
            .type_aliases
            .iter()
            .filter(|(_, v)| v.publicity.is_public())
        {
            let _ = type_aliases.insert(
                name.clone(),
                TypeAliasInterface {
                    documentation: alias.documentation.clone(),
                    deprecation: DeprecationInterface::from_deprecation(&alias.deprecation),
                    parameters: alias.arity,
                    alias: TypeInterface::from_type(&alias.type_),
                },
            );
        }

        for (name, value) in interface
            .values
            .iter()
            .filter(|(_, v)| v.publicity.is_public())
        {
            match (value.type_.as_ref(), value.variant.clone()) {
                (
                    Type::Fn {
                        arguments,
                        return_: return_type,
                    },
                    ValueConstructorVariant::ModuleFn {
                        documentation,
                        implementations,
                        field_map,
                        ..
                    },
                ) => {
                    let mut id_map = IdMap::new();

                    let reverse_field_map = field_map
                        .as_ref()
                        .map(|field_map| field_map.indices_to_labels())
                        .unwrap_or_default();

                    let _ = functions.insert(
                        name.clone(),
                        FunctionInterface {
                            implementations: ImplementationsInterface::from_implementations(
                                &implementations,
                            ),
                            deprecation: DeprecationInterface::from_deprecation(&value.deprecation),
                            documentation,
                            parameters: arguments
                                .iter()
                                .enumerate()
                                .map(|(index, type_)| ParameterInterface {
                                    label: reverse_field_map
                                        .get(&(index as u32))
                                        .map(|label| (*label).clone()),
                                    type_: from_type_helper(type_, &mut id_map),
                                })
                                .collect(),
                            return_: from_type_helper(return_type, &mut id_map),
                        },
                    );
                }

                (
                    type_,
                    ValueConstructorVariant::ModuleConstant {
                        documentation,
                        implementations,
                        ..
                    },
                ) => {
                    let _ = constants.insert(
                        name.clone(),
                        ConstantInterface {
                            implementations: ImplementationsInterface::from_implementations(
                                &implementations,
                            ),
                            type_: TypeInterface::from_type(type_),
                            deprecation: DeprecationInterface::from_deprecation(&value.deprecation),
                            documentation,
                        },
                    );
                }

                _ => {}
            }
        }

        ModuleInterface {
            documentation: interface.documentation.clone(),
            types,
            type_aliases,
            constants,
            functions,
        }
    }
}

impl TypeInterface {
    fn from_type(type_: &Type) -> TypeInterface {
        from_type_helper(type_, &mut IdMap::new())
    }
}

/// Turns a type into its interface, an `IdMap` is needed to make sure that all
/// the type variables' ids that appear in the type are mapped to an incremental
/// number and consistent with each other (that is, two types variables that
/// have the same id will also have the same incremental number in the end).
fn from_type_helper(type_: &Type, id_map: &mut IdMap) -> TypeInterface {
    match type_ {
        Type::Fn { arguments, return_ } => TypeInterface::Fn {
            parameters: arguments
                .iter()
                .map(|argument| from_type_helper(argument.as_ref(), id_map))
                .collect(),
            return_: Box::new(from_type_helper(return_, id_map)),
        },

        Type::Tuple { elements } => TypeInterface::Tuple {
            elements: elements
                .iter()
                .map(|element| from_type_helper(element.as_ref(), id_map))
                .collect(),
        },

        Type::Var { type_ } => match type_
            .as_ref()
            .try_borrow()
            .expect("borrow type after inference")
            .deref()
        {
            TypeVar::Link { type_ } => from_type_helper(type_, id_map),
            // Since package serialisation happens after inference there
            // should be no unbound type variables.
            // TODO: This branch should be `unreachable!()` but because of
            //       https://github.com/gleam-lang/gleam/issues/2533
            //       we sometimes end up with those in top level
            //       definitions.
            //       However, `Unbound` and `Generic` ids are generated
            //       using the same generator so we have no problem treating
            //       unbound variables as generic ones since ids will never
            //       overlap.
            //       Once #2533 is closed this branch can be turned back to
            //       be unreachable!().
            TypeVar::Unbound { id } | TypeVar::Generic { id } => TypeInterface::Variable {
                id: id_map.map_id(*id),
            },
        },

        Type::Named {
            name,
            module,
            arguments,
            package,
            ..
        } => TypeInterface::Named {
            name: name.clone(),
            package: package.clone(),
            module: module.clone(),
            parameters: arguments
                .iter()
                .map(|argument| from_type_helper(argument.as_ref(), id_map))
                .collect(),
        },
    }
}

/// This is a map that is used to map type variable id's to progressive numbers
/// starting from 0.
/// After type inference the ids associated with type variables can be quite
/// high and are not the best to produce a human/machine readable output.
///
/// Imagine a function like this one: `pub fn wibble(item: a, rest: b) -> c`
/// What we want here is for type variables to have increasing ids starting from
/// 0: `a` with id `0`, `b` with id `1` and `c` with id `2`.
///
/// This map allows us to keep track of the ids we've run into and map those to
/// their incremental counterpart starting from 0.
struct IdMap {
    next_id: u64,
    ids: HashMap<u64, u64>,
}

impl IdMap {
    /// Create a new map that will assign id numbers starting from 0.
    fn new() -> IdMap {
        IdMap {
            next_id: 0,
            ids: HashMap::new(),
        }
    }

    /// Map an id to its mapped counterpart starting from 0. If an id has never
    /// been seen before it will be assigned a new incremental number.
    fn map_id(&mut self, id: u64) -> u64 {
        match self.ids.get(&id) {
            Some(mapped_id) => *mapped_id,
            None => {
                let mapped_id = self.next_id;
                let _ = self.ids.insert(id, mapped_id);
                self.next_id += 1;
                mapped_id
            }
        }
    }

    /// If the type is a type variable, and has not been seen before, it will
    /// be assigned to a new incremental number.
    fn add_type_variable_id(&mut self, type_: &Type) {
        match type_ {
            // These types have no id to add to the map.
            Type::Named { .. } | Type::Fn { .. } | Type::Tuple { .. } => (),
            // If the type is actually a type variable whose id needs to be mapped.
            Type::Var { type_ } => match type_
                .as_ref()
                .try_borrow()
                .expect("borrow type after inference")
                .deref()
            {
                TypeVar::Link { .. } => (),
                TypeVar::Unbound { id } | TypeVar::Generic { id } => {
                    let _ = self.map_id(*id);
                }
            },
        }
    }
}
