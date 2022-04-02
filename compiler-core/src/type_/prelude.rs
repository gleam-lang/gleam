use crate::{ast::SrcSpan, build::Origin, uid::UniqueIdGenerator};

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

pub fn build_prelude(ids: &UniqueIdGenerator) -> Module {
    let value = |variant, type_| ValueConstructor {
        public: true,
        variant,
        type_,
    };

    let mut prelude = Module {
        name: vec!["gleam".to_string()],
        package: "".to_string(),
        origin: Origin::Src,
        types: HashMap::new(),
        types_constructors: HashMap::new(),
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

    let _ = prelude.types_constructors.insert(
        "Bool".to_string(),
        vec!["True".to_string(), "False".to_string()],
    );

    let _ = prelude.values.insert(
        "True".to_string(),
        value(
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "True".to_string(),
                field_map: None,
                arity: 0,
                location: SrcSpan::default(),
            },
            bool(),
        ),
    );
    let _ = prelude.values.insert(
        "False".to_string(),
        value(
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "False".to_string(),
                field_map: None,
                arity: 0,
                location: SrcSpan::default(),
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

    let list_parameter = generic_var(ids.next());
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

    let result_value = generic_var(ids.next());
    let result_error = generic_var(ids.next());
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

    let _ = prelude.types_constructors.insert(
        "Result".to_string(),
        vec!["Ok".to_string(), "Error".to_string()],
    );

    let _ = prelude.values.insert(
        "Nil".to_string(),
        value(
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "Nil".to_string(),
                arity: 0,
                field_map: None,
                location: SrcSpan::default(),
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

    let ok = generic_var(ids.next());
    let error = generic_var(ids.next());
    let _ = prelude.values.insert(
        "Ok".to_string(),
        value(
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "Ok".to_string(),
                field_map: None,
                arity: 1,
                location: SrcSpan::default(),
            },
            fn_(vec![ok.clone()], result(ok, error)),
        ),
    );

    let ok = generic_var(ids.next());
    let error = generic_var(ids.next());
    let _ = prelude.values.insert(
        "Error".to_string(),
        value(
            ValueConstructorVariant::Record {
                module: "".into(),
                name: "Error".to_string(),
                field_map: None,
                arity: 1,
                location: SrcSpan::default(),
            },
            fn_(vec![error.clone()], result(ok, error)),
        ),
    );

    prelude
}
