use std::{collections::HashMap, ops::Deref};

use ecow::EcoString;
use hexpm::version::Version;
use serde::Serialize;

#[cfg(test)]
mod tests;

use crate::{
    ast::{CustomType, Definition, Function, ModuleConstant},
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
    types: HashMap<EcoString, TypeDefinitionInterface>,
    #[serde(serialize_with = "ordered_map")]
    values: HashMap<EcoString, ValueInterface>,
}

/// Holds serialisable data about a type definition that appears in a module.
#[derive(Serialize, Debug)]
pub struct TypeDefinitionInterface {
    documentation: Option<EcoString>,
    deprecation: Deprecation,
    /// The number of the type's type variables
    parameters: usize,
    /// A list of the names of its constructors
    constructors: Vec<EcoString>,
}

/// Holds serialisable data about a top-level public value defined in a module
/// (either a constant or a function)
#[derive(Serialize, Debug)]
pub struct ValueInterface {
    documentation: Option<EcoString>,
    deprecation: Deprecation,
    supported_targets: Vec<Target>,
    #[serde(rename = "type")]
    type_: TypeInterface,
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
    /// A generic type variable.
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
        let (types, values) = statements_interfaces(module);
        ModuleInterface {
            documentation: module.ast.documentation.clone(),
            types,
            values,
        }
    }
}

fn statements_interfaces(
    module: &Module,
) -> (
    HashMap<EcoString, TypeDefinitionInterface>,
    HashMap<EcoString, ValueInterface>,
) {
    let mut types = HashMap::new();
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
                        deprecation: deprecation.clone(),
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
                        deprecation: Deprecation::NotDeprecated,
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
                let _ = values.insert(
                    name.clone(),
                    ValueInterface {
                        supported_targets: supported_targets.to_vec(),
                        deprecation: deprecation.clone(),
                        documentation: documentation.clone(),
                        type_: TypeInterface::Fn {
                            parameters: arguments
                                .iter()
                                .map(|arg| TypeInterface::from_type(arg.type_.as_ref()))
                                .collect(),
                            return_: Box::new(TypeInterface::from_type(return_type)),
                        },
                    },
                );
            }

            // Private definitions are not included.
            Definition::Function(_) => {}
            Definition::CustomType(_) => {}
            Definition::ModuleConstant(_) => {}

            // Type aliases are ignored since those are also erased from the
            // generated documentation.
            Definition::TypeAlias(_) => {}

            // Imports are ignored.
            Definition::Import(_) => {}
        }
    }
    (types, values)
}

impl TypeInterface {
    fn from_type(type_: &Type) -> TypeInterface {
        match type_ {
            Type::Fn { args, retrn } => TypeInterface::Fn {
                parameters: args
                    .iter()
                    .map(|arg| TypeInterface::from_type(arg.as_ref()))
                    .collect(),
                return_: Box::new(TypeInterface::from_type(retrn)),
            },

            Type::Tuple { elems } => TypeInterface::Tuple {
                elements: elems
                    .iter()
                    .map(|elem| TypeInterface::from_type(elem.as_ref()))
                    .collect(),
            },

            Type::Var { type_ } => match type_.as_ref().try_borrow() {
                // Package serialisation happens after inference has taken place
                // At this point no one should be helding a mutable reference of
                // the type: the borrow should never fail.
                Err(_) => unreachable!(),
                Ok(ref_) => match ref_.deref() {
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
                    TypeVar::Unbound { id } => TypeInterface::Variable { id: *id },
                    TypeVar::Link { type_ } => TypeInterface::from_type(type_),
                    TypeVar::Generic { id } => TypeInterface::Variable { id: *id },
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
                    .map(|arg| TypeInterface::from_type(arg.as_ref()))
                    .collect(),
            },
        }
    }
}
