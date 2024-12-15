use hexpm::version::Version;
use strum::{EnumIter, IntoEnumIterator};

use crate::{
    ast::{Publicity, SrcSpan},
    build::Origin,
    line_numbers::LineNumbers,
    uid::UniqueIdGenerator,
};

use super::{
    FunctionArgument, ModuleInterface, Type, TypeConstructor, TypeValueConstructor,
    TypeValueConstructorField, TypeVar, TypeVariantConstructors, ValueConstructor,
    ValueConstructorVariant,
};
use crate::type_::Deprecation::NotDeprecated;
use std::{cell::RefCell, collections::HashMap, sync::Arc};

const BIT_ARRAY: &str = "BitArray";
const BOOL: &str = "Bool";
const FLOAT: &str = "Float";
const INT: &str = "Int";
const LIST: &str = "List";
const NIL: &str = "Nil";
const RESULT: &str = "Result";
const STRING: &str = "String";
const UTF_CODEPOINT: &str = "UtfCodepoint";

pub const PRELUDE_PACKAGE_NAME: &str = "";
pub const PRELUDE_MODULE_NAME: &str = "gleam";

pub fn is_prelude_module(module: &str) -> bool {
    module == PRELUDE_MODULE_NAME
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
pub enum PreludeType {
    BitArray,
    Bool,
    Float,
    Int,
    List,
    Nil,
    Result,
    String,
    UtfCodepoint,
}

impl PreludeType {
    pub fn name(self) -> &'static str {
        match self {
            PreludeType::BitArray => BIT_ARRAY,
            PreludeType::Bool => BOOL,
            PreludeType::Float => FLOAT,
            PreludeType::Int => INT,
            PreludeType::List => LIST,
            PreludeType::Nil => NIL,
            PreludeType::Result => RESULT,
            PreludeType::String => STRING,
            PreludeType::UtfCodepoint => UTF_CODEPOINT,
        }
    }
}

pub fn int() -> Arc<Type> {
    Arc::new(Type::Named {
        publicity: Publicity::Public,
        name: INT.into(),
        module: PRELUDE_MODULE_NAME.into(),
        package: PRELUDE_PACKAGE_NAME.into(),
        args: vec![],
    })
}

pub fn float() -> Arc<Type> {
    Arc::new(Type::Named {
        args: vec![],
        publicity: Publicity::Public,
        name: FLOAT.into(),
        module: PRELUDE_MODULE_NAME.into(),
        package: PRELUDE_PACKAGE_NAME.into(),
    })
}

pub fn bool() -> Arc<Type> {
    Arc::new(Type::Named {
        args: vec![],
        publicity: Publicity::Public,
        name: BOOL.into(),
        module: PRELUDE_MODULE_NAME.into(),
        package: PRELUDE_PACKAGE_NAME.into(),
    })
}

pub fn string() -> Arc<Type> {
    Arc::new(Type::Named {
        args: vec![],
        publicity: Publicity::Public,
        name: STRING.into(),
        module: PRELUDE_MODULE_NAME.into(),
        package: PRELUDE_PACKAGE_NAME.into(),
    })
}

pub fn nil() -> Arc<Type> {
    Arc::new(Type::Named {
        args: vec![],
        publicity: Publicity::Public,
        name: NIL.into(),
        module: PRELUDE_MODULE_NAME.into(),
        package: PRELUDE_PACKAGE_NAME.into(),
    })
}

pub fn list(t: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::Named {
        publicity: Publicity::Public,
        name: LIST.into(),
        module: PRELUDE_MODULE_NAME.into(),
        package: PRELUDE_PACKAGE_NAME.into(),
        args: vec![t],
    })
}

pub fn result(a: Arc<Type>, e: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::Named {
        publicity: Publicity::Public,
        name: RESULT.into(),
        module: PRELUDE_MODULE_NAME.into(),
        package: PRELUDE_PACKAGE_NAME.into(),
        args: vec![a, e],
    })
}

pub fn tuple(elems: Vec<Arc<Type>>) -> Arc<Type> {
    Arc::new(Type::Tuple { elems })
}

pub fn fn_(args: Vec<FunctionArgument>, retrn: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::Fn { retrn, args })
}

pub fn named(
    package: &str,
    module: &str,
    name: &str,
    publicity: Publicity,
    args: Vec<Arc<Type>>,
) -> Arc<Type> {
    Arc::new(Type::Named {
        publicity,
        package: package.into(),
        module: module.into(),
        name: name.into(),
        args,
    })
}

pub fn bits() -> Arc<Type> {
    Arc::new(Type::Named {
        args: vec![],
        publicity: Publicity::Public,
        name: BIT_ARRAY.into(),
        module: PRELUDE_MODULE_NAME.into(),
        package: PRELUDE_PACKAGE_NAME.into(),
    })
}

pub fn utf_codepoint() -> Arc<Type> {
    Arc::new(Type::Named {
        args: vec![],
        publicity: Publicity::Public,
        name: UTF_CODEPOINT.into(),
        module: PRELUDE_MODULE_NAME.into(),
        package: PRELUDE_PACKAGE_NAME.into(),
    })
}

pub fn generic_var(id: u64) -> Arc<Type> {
    Arc::new(Type::Var {
        type_: Arc::new(RefCell::new(TypeVar::Generic { id })),
    })
}

pub fn unbound_var(id: u64) -> Arc<Type> {
    Arc::new(Type::Var {
        type_: Arc::new(RefCell::new(TypeVar::Unbound { id })),
    })
}

#[cfg(test)]
pub fn link(type_: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::Var {
        type_: Arc::new(RefCell::new(TypeVar::Link { type_ })),
    })
}

pub fn build_prelude(ids: &UniqueIdGenerator) -> ModuleInterface {
    let value = |variant, type_| ValueConstructor {
        publicity: Publicity::Public,
        deprecation: NotDeprecated,
        variant,
        type_,
    };

    let mut prelude = ModuleInterface {
        name: PRELUDE_MODULE_NAME.into(),
        package: "".into(),
        origin: Origin::Src,
        documentation: vec![],
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        type_aliases: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        is_internal: false,
        warnings: vec![],
        // prelude doesn't have real src
        src_path: "".into(),
        // prelude doesn't have real line numbers
        line_numbers: LineNumbers::new(""),
        minimum_required_version: Version::new(0, 1, 0),
    };

    for t in PreludeType::iter() {
        match t {
            PreludeType::BitArray => {
                let v = TypeConstructor {
                    origin: Default::default(),
                    parameters: vec![],
                    type_: bits(),
                    module: PRELUDE_MODULE_NAME.into(),
                    publicity: Publicity::Public,
                    opaque: false,
                    deprecation: NotDeprecated,
                    documentation: None,
                };
                let _ = prelude.types.insert(BIT_ARRAY.into(), v.clone());
            }

            PreludeType::Bool => {
                let _ = prelude.types_value_constructors.insert(
                    BOOL.into(),
                    TypeVariantConstructors {
                        type_parameters_ids: vec![],
                        variants: vec![
                            TypeValueConstructor {
                                name: "True".into(),
                                parameters: vec![],
                            },
                            TypeValueConstructor {
                                name: "False".into(),
                                parameters: vec![],
                            },
                        ],
                    },
                );
                let _ = prelude.values.insert(
                    "True".into(),
                    value(
                        ValueConstructorVariant::Record {
                            documentation: None,
                            module: PRELUDE_MODULE_NAME.into(),
                            name: "True".into(),
                            field_map: None,
                            arity: 0,
                            location: SrcSpan::default(),
                            constructors_count: 2,
                            constructor_index: 0,
                        },
                        bool(),
                    ),
                );
                let _ = prelude.values.insert(
                    "False".into(),
                    value(
                        ValueConstructorVariant::Record {
                            documentation: None,
                            module: PRELUDE_MODULE_NAME.into(),
                            name: "False".into(),
                            field_map: None,
                            arity: 0,
                            location: SrcSpan::default(),
                            constructors_count: 2,
                            constructor_index: 1,
                        },
                        bool(),
                    ),
                );
                let _ = prelude.types.insert(
                    BOOL.into(),
                    TypeConstructor {
                        origin: Default::default(),
                        parameters: vec![],
                        type_: bool(),
                        module: PRELUDE_MODULE_NAME.into(),
                        publicity: Publicity::Public,
                        opaque: false,
                        deprecation: NotDeprecated,
                        documentation: None,
                    },
                );
            }

            PreludeType::Float => {
                let _ = prelude.types.insert(
                    FLOAT.into(),
                    TypeConstructor {
                        origin: Default::default(),
                        parameters: vec![],
                        type_: float(),
                        module: PRELUDE_MODULE_NAME.into(),
                        publicity: Publicity::Public,
                        opaque: false,
                        deprecation: NotDeprecated,
                        documentation: None,
                    },
                );
            }

            PreludeType::Int => {
                let _ = prelude.types.insert(
                    INT.into(),
                    TypeConstructor {
                        parameters: vec![],
                        type_: int(),
                        origin: Default::default(),
                        module: PRELUDE_MODULE_NAME.into(),
                        publicity: Publicity::Public,
                        opaque: false,
                        deprecation: NotDeprecated,
                        documentation: None,
                    },
                );
            }

            PreludeType::List => {
                let list_parameter = generic_var(ids.next());
                let _ = prelude.types.insert(
                    LIST.into(),
                    TypeConstructor {
                        origin: Default::default(),
                        parameters: vec![list_parameter.clone()],
                        type_: list(list_parameter),
                        module: PRELUDE_MODULE_NAME.into(),
                        publicity: Publicity::Public,
                        opaque: false,
                        deprecation: NotDeprecated,
                        documentation: None,
                    },
                );
            }

            PreludeType::Nil => {
                let _ = prelude.values.insert(
                    NIL.into(),
                    value(
                        ValueConstructorVariant::Record {
                            documentation: None,
                            module: PRELUDE_MODULE_NAME.into(),
                            name: NIL.into(),
                            arity: 0,
                            field_map: None,
                            location: SrcSpan::default(),
                            constructors_count: 1,
                            constructor_index: 0,
                        },
                        nil(),
                    ),
                );
                let _ = prelude.types.insert(
                    NIL.into(),
                    TypeConstructor {
                        origin: Default::default(),
                        parameters: vec![],
                        type_: nil(),
                        module: PRELUDE_MODULE_NAME.into(),
                        publicity: Publicity::Public,
                        opaque: false,
                        deprecation: NotDeprecated,
                        documentation: None,
                    },
                );
                let _ = prelude.types_value_constructors.insert(
                    NIL.into(),
                    TypeVariantConstructors {
                        type_parameters_ids: vec![],
                        variants: vec![TypeValueConstructor {
                            name: "Nil".into(),
                            parameters: vec![],
                        }],
                    },
                );
            }

            PreludeType::Result => {
                let result_value_id = ids.next();
                let result_error_id = ids.next();
                let result_value = generic_var(result_value_id);
                let result_error = generic_var(result_error_id);
                let _ = prelude.types.insert(
                    RESULT.into(),
                    TypeConstructor {
                        origin: Default::default(),
                        parameters: vec![result_value.clone(), result_error.clone()],
                        type_: result(result_value.clone(), result_error.clone()),
                        module: PRELUDE_MODULE_NAME.into(),
                        publicity: Publicity::Public,
                        opaque: false,
                        deprecation: NotDeprecated,
                        documentation: None,
                    },
                );
                let _ = prelude.types_value_constructors.insert(
                    RESULT.into(),
                    TypeVariantConstructors {
                        type_parameters_ids: vec![result_value_id, result_error_id],
                        variants: vec![
                            TypeValueConstructor {
                                name: "Ok".into(),
                                parameters: vec![TypeValueConstructorField {
                                    label: None,
                                    type_: result_value,
                                }],
                            },
                            TypeValueConstructor {
                                name: "Error".into(),
                                parameters: vec![TypeValueConstructorField {
                                    label: None,
                                    type_: result_error,
                                }],
                            },
                        ],
                    },
                );
                let ok = generic_var(ids.next());
                let error = generic_var(ids.next());
                let _ = prelude.values.insert(
                    "Ok".into(),
                    value(
                        ValueConstructorVariant::Record {
                            documentation: None,
                            module: PRELUDE_MODULE_NAME.into(),
                            name: "Ok".into(),
                            field_map: None,
                            arity: 1,
                            location: SrcSpan::default(),
                            constructors_count: 2,
                            constructor_index: 0,
                        },
                        fn_(
                            vec![FunctionArgument {
                                name: None,
                                type_: ok.clone(),
                            }],
                            result(ok, error),
                        ),
                    ),
                );
                let ok = generic_var(ids.next());
                let error = generic_var(ids.next());
                let _ = prelude.values.insert(
                    "Error".into(),
                    value(
                        ValueConstructorVariant::Record {
                            documentation: None,
                            module: PRELUDE_MODULE_NAME.into(),
                            name: "Error".into(),
                            field_map: None,
                            arity: 1,
                            location: SrcSpan::default(),
                            constructors_count: 2,
                            constructor_index: 1,
                        },
                        fn_(
                            vec![FunctionArgument {
                                name: None,
                                type_: error.clone(),
                            }],
                            result(ok, error),
                        ),
                    ),
                );
            }

            PreludeType::String => {
                let _ = prelude.types.insert(
                    STRING.into(),
                    TypeConstructor {
                        origin: Default::default(),
                        parameters: vec![],
                        type_: string(),
                        module: PRELUDE_MODULE_NAME.into(),
                        publicity: Publicity::Public,
                        opaque: false,
                        deprecation: NotDeprecated,
                        documentation: None,
                    },
                );
            }

            PreludeType::UtfCodepoint => {
                let _ = prelude.types.insert(
                    UTF_CODEPOINT.into(),
                    TypeConstructor {
                        origin: Default::default(),
                        parameters: vec![],
                        type_: utf_codepoint(),
                        module: PRELUDE_MODULE_NAME.into(),
                        publicity: Publicity::Public,
                        opaque: false,
                        deprecation: NotDeprecated,
                        documentation: None,
                    },
                );
            }
        }
    }

    prelude
}
