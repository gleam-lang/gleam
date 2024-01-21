use std::{collections::HashMap, ops::Deref};

use ecow::EcoString;
use hexpm::version::Version;
use itertools::Itertools;
use serde::Serialize;

#[cfg(test)]
mod tests;

use crate::{
    ast::{CustomType, Definition, Function, ModuleConstant, TypeAlias},
    manifest::ordered_map,
    type_::{expression::Implementations, Deprecation, Type, TypeVar},
};

use crate::build::{Module, Package};

#[derive(Serialize, Debug)]
pub struct PackageInterface {
    name: EcoString,
    package_version: Version,
    gleam_version: Option<EcoString>,
    #[serde(serialize_with = "ordered_map")]
    modules: HashMap<EcoString, ModuleInterface>,
}

/// Holds serialisable data about a `Module`.
#[derive(Serialize, Debug)]
pub struct ModuleInterface {
    documentation: Vec<EcoString>,
    #[serde(serialize_with = "ordered_map")]
    type_aliases: HashMap<EcoString, TypeAliasInterface>,
    #[serde(serialize_with = "ordered_map")]
    types: HashMap<EcoString, TypeDefinitionInterface>,
    #[serde(serialize_with = "ordered_map")]
    values: HashMap<EcoString, ValueInterface>,
}

/// Holds serialisable data about a type definition that appears in a module.
#[derive(Serialize, Debug)]
pub struct TypeDefinitionInterface {
    documentation: Option<EcoString>,
    deprecation: Option<DeprecationInterface>,
    /// The number of the type's type variables
    parameters: usize,
    /// A list of the names of its constructors
    constructors: Vec<TypeConstructorInterface>,
}

#[derive(Serialize, Debug)]
pub struct TypeConstructorInterface {
    documentation: Option<EcoString>,
    name: EcoString,
    parameters: Vec<ParameterInterface>,
}

/// Holds serialisable data about a type alias that appears in a module.
#[derive(Serialize, Debug)]
pub struct TypeAliasInterface {
    documentation: Option<EcoString>,
    deprecation: Option<DeprecationInterface>,
    parameters: usize,
    alias: TypeInterface,
}

/// Holds serialisable data about a top-level public value defined in a module
/// (either a constant or a function)
#[derive(Serialize, Debug)]
pub struct ValueInterface {
    documentation: Option<EcoString>,
    deprecation: Option<DeprecationInterface>,
    implementations: Implementations,
    #[serde(rename = "type")]
    type_: TypeInterface,
}

#[derive(Serialize, Debug)]
pub struct DeprecationInterface {
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

/// This is the serialisable version of a type of a top-level value of a module.
#[derive(Serialize, Debug)]
#[serde(tag = "kind")]
pub enum TypeInterface {
    Tuple {
        elements: Vec<TypeInterface>,
    },
    Fn {
        parameters: Vec<ParameterInterface>,
        #[serde(rename = "return")]
        return_: Box<TypeInterface>,
    },
    /// A type variable.
    Variable {
        id: u64,
    },
    /// A custom named type.
    Named {
        name: EcoString,
        package: EcoString,
        module: EcoString,
        parameters: Vec<TypeInterface>,
    },
}

#[derive(Serialize, Debug)]
pub struct ParameterInterface {
    label: Option<EcoString>,
    #[serde(rename = "type")]
    type_: TypeInterface,
}

impl PackageInterface {
    pub fn from_package(package: &Package) -> PackageInterface {
        PackageInterface {
            name: package.config.name.clone(),
            package_version: package.config.version.clone(),
            gleam_version: package.config.gleam_version.clone(),
            modules: package
                .modules
                .iter()
                .map(|module| (module.name.clone(), ModuleInterface::from_module(module)))
                .collect(),
        }
    }
}

impl ModuleInterface {
    fn from_module(module: &Module) -> ModuleInterface {
        let (types, type_aliases, values) = statements_interfaces(module);
        ModuleInterface {
            documentation: module.ast.documentation.clone(),
            types,
            type_aliases,
            values,
        }
    }
}

fn statements_interfaces(
    module: &Module,
) -> (
    HashMap<EcoString, TypeDefinitionInterface>,
    HashMap<EcoString, TypeAliasInterface>,
    HashMap<EcoString, ValueInterface>,
) {
    let mut types = HashMap::new();
    let mut type_aliases = HashMap::new();
    let mut values = HashMap::new();
    for statement in &module.ast.definitions {
        match statement {
            // A public type definition.
            Definition::CustomType(CustomType {
                public: true,
                name,
                constructors,
                documentation,
                opaque,
                deprecation,
                typed_parameters,
                parameters: _,
                location: _,
                end_position: _,
            }) => {
                let mut id_map = IdMap::new();

                // Let's first add all the types that appear in the type parameters so those are
                // taken into account when assigning incremental numbers to the constructor's
                // type variables.
                for typed_parameter in typed_parameters {
                    id_map.add_type_variable_id(typed_parameter.as_ref());
                }

                let _ = types.insert(
                    name.clone(),
                    TypeDefinitionInterface {
                        documentation: documentation.clone(),
                        deprecation: DeprecationInterface::from_deprecation(deprecation),
                        parameters: typed_parameters.len(),
                        constructors: if *opaque {
                            vec![]
                        } else {
                            constructors
                                .iter()
                                .map(|constructor| TypeConstructorInterface {
                                    documentation: constructor.documentation.clone(),
                                    name: constructor.name.clone(),
                                    parameters: constructor
                                        .arguments
                                        .iter()
                                        .map(|arg| ParameterInterface {
                                            label: arg.label.clone(),
                                            // We share the same id_map between each step so that the
                                            // incremental ids assigned are consisten with each other
                                            type_: from_type_helper(&arg.type_, &mut id_map),
                                        })
                                        .collect_vec(),
                                })
                                .collect()
                        },
                    },
                );
            }

            // A public type alias definition
            Definition::TypeAlias(TypeAlias {
                public: true,
                alias,
                parameters,
                type_,
                documentation,
                deprecation,
                location: _,
                type_ast: _,
            }) => {
                let _ = type_aliases.insert(
                    alias.clone(),
                    TypeAliasInterface {
                        documentation: documentation.clone(),
                        deprecation: DeprecationInterface::from_deprecation(deprecation),
                        parameters: parameters.len(),
                        alias: TypeInterface::from_type(type_.as_ref()),
                    },
                );
            }

            // A public module constant.
            Definition::ModuleConstant(ModuleConstant {
                public: true,
                name,
                type_,
                documentation,
                implementations,
                deprecation,
                location: _,
                annotation: _,
                value: _,
            }) => {
                let _ = values.insert(
                    name.clone(),
                    ValueInterface {
                        implementations: implementations.clone(),
                        type_: TypeInterface::from_type(type_.as_ref()),
                        deprecation: DeprecationInterface::from_deprecation(deprecation),
                        documentation: documentation.clone(),
                    },
                );
            }

            // A public top-level function.
            Definition::Function(Function {
                public: true,
                name,
                arguments,
                deprecation,
                return_type,
                documentation,
                implementations,
                location: _,
                end_position: _,
                body: _,
                return_annotation: _,
                external_erlang: _,
                external_javascript: _,
            }) => {
                let mut id_map = IdMap::new();
                let _ = values.insert(
                    name.clone(),
                    ValueInterface {
                        implementations: implementations.clone(),
                        deprecation: DeprecationInterface::from_deprecation(deprecation),
                        documentation: documentation.clone(),
                        type_: TypeInterface::Fn {
                            // We use the same id_map between parameters and return type so that
                            // type variables have consistent ids between the two.
                            parameters: arguments
                                .iter()
                                .map(|arg| ParameterInterface {
                                    label: arg.names.get_label().cloned(),
                                    type_: from_type_helper(arg.type_.as_ref(), &mut id_map),
                                })
                                .collect(),
                            return_: Box::new(from_type_helper(return_type, &mut id_map)),
                        },
                    },
                );
            }

            // Private definitions are not included.
            Definition::Function(_) => {}
            Definition::CustomType(_) => {}
            Definition::ModuleConstant(_) => {}
            Definition::TypeAlias(_) => {}

            // Imports are ignored.
            Definition::Import(_) => {}
        }
    }
    (types, type_aliases, values)
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
        Type::Fn { args, retrn } => TypeInterface::Fn {
            parameters: args
                .iter()
                .map(|arg| ParameterInterface {
                    label: None,
                    type_: from_type_helper(arg.as_ref(), id_map),
                })
                .collect(),
            return_: Box::new(from_type_helper(retrn, id_map)),
        },

        Type::Tuple { elems } => TypeInterface::Tuple {
            elements: elems
                .iter()
                .map(|elem| from_type_helper(elem.as_ref(), id_map))
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
            args,
            package,
            ..
        } => TypeInterface::Named {
            name: name.clone(),
            package: package.clone(),
            module: module.clone(),
            parameters: args
                .iter()
                .map(|arg| from_type_helper(arg.as_ref(), id_map))
                .collect(),
        },
    }
}

/// This is a map that is used to map type variable id's to progressive numbers
/// starting from 0.
/// After type inference the ids associated with type variables can be quite
/// high and are not the best to produce a human/machine readable output.
///
/// Imagine a function like this one: `pub fn foo(item: a, rest: b) -> c`
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
