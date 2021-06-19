use crate::build::Origin;

use super::{Module, Type, TypeConstructor, TypeVar, ValueConstructor, ValueConstructorVariant};
use std::{cell::RefCell, collections::HashMap, sync::Arc};

pub fn int() -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: "Int".to_string(),
        module: vec![],
        args: vec![],
    })
}

pub fn float() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: "Float".to_string(),
        module: vec![],
    })
}

pub fn bool() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: "Bool".to_string(),
        module: vec![],
    })
}

pub fn string() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: "String".to_string(),
        module: vec![],
    })
}

pub fn nil() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: "Nil".to_string(),
        module: vec![],
    })
}

pub fn list(t: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: "List".to_string(),
        module: vec![],
        args: vec![t],
    })
}

pub fn result(a: Arc<Type>, e: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: "Result".to_string(),
        module: vec![],
        args: vec![a, e],
    })
}

pub fn tuple(elems: Vec<Arc<Type>>) -> Arc<Type> {
    Arc::new(Type::Tuple { elems })
}

pub fn fn_(args: Vec<Arc<Type>>, retrn: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::Fn { retrn, args })
}

pub fn bit_string() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: "BitString".to_string(),
        module: vec![],
    })
}

pub fn utf_codepoint() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: "UtfCodepoint".to_string(),
        module: vec![],
    })
}

pub fn generic_var(id: usize) -> Arc<Type> {
    Arc::new(Type::Var {
        type_: Arc::new(RefCell::new(TypeVar::Generic { id })),
    })
}

pub fn unbound_var(id: usize, level: usize) -> Arc<Type> {
    Arc::new(Type::Var {
        type_: Arc::new(RefCell::new(TypeVar::Unbound { id, level })),
    })
}

#[cfg(test)]
pub fn link(type_: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::Var {
        type_: Arc::new(RefCell::new(TypeVar::Link { type_ })),
    })
}

pub fn build_prelude(uid: &mut usize) -> Module {
    let mut new_generic_var = || {
        let t = generic_var(*uid);
        *uid += 1;
        t
    };

    let value = |variant, type_| ValueConstructor {
        public: true,
        origin: Default::default(),
        variant,
        type_,
    };

    let mut prelude = Module {
        name: vec!["gleam".to_string()],
        package: "".to_string(),
        origin: Origin::Src,
        types: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };

    let _ = prelude.types.insert(
        "Int".to_string(),
        TypeConstructor {
            parameters: vec![],
            typ: int(),
            origin: Default::default(),
            module: vec![],
            public: true,
        },
    );

    let _ = prelude.values.insert(
        "True".to_string(),
        value(
            ValueConstructorVariant::Record {
                name: "True".to_string(),
                field_map: None,
                arity: 0,
            },
            bool(),
        ),
    );
    let _ = prelude.values.insert(
        "False".to_string(),
        value(
            ValueConstructorVariant::Record {
                name: "False".to_string(),
                field_map: None,
                arity: 0,
            },
            bool(),
        ),
    );
    let _ = prelude.types.insert(
        "Bool".to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: bool(),
            module: vec![],
            public: true,
        },
    );

    let list_parameter = new_generic_var();
    let _ = prelude.types.insert(
        "List".to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![list_parameter.clone()],
            typ: list(list_parameter),
            module: vec![],
            public: true,
        },
    );

    let _ = prelude.types.insert(
        "Float".to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: float(),
            module: vec![],
            public: true,
        },
    );

    let _ = prelude.types.insert(
        "String".to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: string(),
            module: vec![],
            public: true,
        },
    );

    let result_value = new_generic_var();
    let result_error = new_generic_var();
    let _ = prelude.types.insert(
        "Result".to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![result_value.clone(), result_error.clone()],
            typ: result(result_value, result_error),
            module: vec![],
            public: true,
        },
    );

    let _ = prelude.values.insert(
        "Nil".to_string(),
        value(
            ValueConstructorVariant::Record {
                name: "Nil".to_string(),
                arity: 0,
                field_map: None,
            },
            nil(),
        ),
    );
    let _ = prelude.types.insert(
        "Nil".to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: nil(),
            module: vec![],
            public: true,
        },
    );

    let _ = prelude.types.insert(
        "BitString".to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: bit_string(),
            module: vec![],
            public: true,
        },
    );

    let _ = prelude.types.insert(
        "UtfCodepoint".to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: utf_codepoint(),
            module: vec![],
            public: true,
        },
    );

    let ok = new_generic_var();
    let error = new_generic_var();
    let _ = prelude.values.insert(
        "Ok".to_string(),
        value(
            ValueConstructorVariant::Record {
                name: "Ok".to_string(),
                field_map: None,
                arity: 1,
            },
            fn_(vec![ok.clone()], result(ok, error)),
        ),
    );

    let ok = new_generic_var();
    let error = new_generic_var();
    let _ = prelude.values.insert(
        "Error".to_string(),
        value(
            ValueConstructorVariant::Record {
                name: "Error".to_string(),
                field_map: None,
                arity: 1,
            },
            fn_(vec![error.clone()], result(ok, error)),
        ),
    );

    prelude
}
