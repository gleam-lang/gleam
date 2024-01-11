use std::{collections::HashMap, ops::Deref};

use ecow::EcoString;
use hexpm::version::Version;
use serde::Serialize;

#[cfg(test)]
mod tests;

use crate::{
    ast::{CustomType, Definition, Function, ModuleConstant, TypeAlias},
    manifest::ordered_map,
    type_::{Deprecation, Type, TypeVar},
};

use crate::build::{Module, Package, Target};

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
    constructors: Vec<EcoString>,
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
    supported_targets: Vec<Target>,
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
pub enum TypeInterface {
    Tuple {
        elements: Vec<TypeInterface>,
    },
    Fn {
        parameters: Vec<TypeInterface>,
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
                parameters,
                location: _,
                end_position: _,
                typed_parameters: _,
            }) => {
                let _ = types.insert(
                    name.clone(),
                    TypeDefinitionInterface {
                        documentation: documentation.clone(),
                        deprecation: DeprecationInterface::from_deprecation(deprecation),
                        parameters: parameters.len(),
                        constructors: if *opaque {
                            vec![]
                        } else {
                            constructors
                                .iter()
                                .map(|constructor| constructor.name.clone())
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
                supported_targets,
                location: _,
                annotation: _,
                value: _,
            }) => {
                let _ = values.insert(
                    name.clone(),
                    ValueInterface {
                        supported_targets: supported_targets.to_vec(),
                        type_: TypeInterface::from_type(type_.as_ref()),
                        deprecation: None,
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
                supported_targets,
                location: _,
                end_position: _,
                body: _,
                return_annotation: _,
                external_erlang: _,
                external_javascript: _,
            }) => {
                let mut ids = HashMap::new();
                let mut next_id = 0;

                let _ = values.insert(
                    name.clone(),
                    ValueInterface {
                        supported_targets: supported_targets.to_vec(),
                        deprecation: DeprecationInterface::from_deprecation(deprecation),
                        documentation: documentation.clone(),
                        type_: TypeInterface::Fn {
                            parameters: arguments
                                .iter()
                                .map(|arg| {
                                    from_type_helper(arg.type_.as_ref(), &mut next_id, &mut ids)
                                })
                                .collect(),
                            return_: Box::new(from_type_helper(
                                return_type,
                                &mut next_id,
                                &mut ids,
                            )),
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
    /// ids is a map that turns a type variable's id into a progressive number
    /// starting from 0, up to `next_id`
    fn from_type(type_: &Type) -> TypeInterface {
        from_type_helper(type_, &mut 0, &mut HashMap::new())
    }
}

fn from_type_helper(type_: &Type, next_id: &mut u64, ids: &mut HashMap<u64, u64>) -> TypeInterface {
    match type_ {
        Type::Fn { args, retrn } => TypeInterface::Fn {
            parameters: args
                .iter()
                .map(|arg| from_type_helper(arg.as_ref(), next_id, ids))
                .collect(),
            return_: Box::new(from_type_helper(retrn, next_id, ids)),
        },

        Type::Tuple { elems } => TypeInterface::Tuple {
            elements: elems
                .iter()
                .map(|elem| from_type_helper(elem.as_ref(), next_id, ids))
                .collect(),
        },

        Type::Var { type_ } => match type_
            .as_ref()
            .try_borrow()
            .expect("borrow type after inference")
            .deref()
        {
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
            TypeVar::Link { type_ } => from_type_helper(type_, next_id, ids),
            TypeVar::Unbound { id } | TypeVar::Generic { id } => match ids.get(id) {
                Some(progressive_id) => TypeInterface::Variable {
                    id: *progressive_id,
                },
                None => {
                    let _ = ids.insert(*id, *next_id);
                    let interface = TypeInterface::Variable { id: *next_id };
                    *next_id = *next_id + 1;
                    interface
                }
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
                .map(|arg| from_type_helper(arg.as_ref(), next_id, ids))
                .collect(),
        },
    }
}
