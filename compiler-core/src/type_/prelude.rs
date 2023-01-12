use crate::{ast::SrcSpan, build::Origin, uid::UniqueIdGenerator};

use super::{Module, Type, TypeConstructor, TypeVar, ValueConstructor, ValueConstructorVariant};
use std::{cell::RefCell, collections::HashMap, sync::Arc};

const BIT_STRING: &str = "BitString";
const BOOL: &str = "Bool";
const FLOAT: &str = "Float";
const INT: &str = "Int";
const LIST: &str = "List";
const NIL: &str = "Nil";
const RESULT: &str = "Result";
const STRING: &str = "String";
const UTF_CODEPOINT: &str = "UtfCodepoint";

// TODO: use "gleam" as the prelude module name, not ""

pub fn int() -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: INT.to_string(),
        module: "".into(),
        args: vec![],
    })
}

pub fn float() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: FLOAT.to_string(),
        module: "".into(),
    })
}

pub fn bool() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: BOOL.to_string(),
        module: "".into(),
    })
}

pub fn string() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: STRING.to_string(),
        module: "".into(),
    })
}

pub fn nil() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: NIL.to_string(),
        module: "".into(),
    })
}

pub fn list(t: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: LIST.to_string(),
        module: "".into(),
        args: vec![t],
    })
}

pub fn result(a: Arc<Type>, e: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: RESULT.to_string(),
        module: "".into(),
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
        name: BIT_STRING.to_string(),
        module: "".into(),
    })
}

pub fn utf_codepoint() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: UTF_CODEPOINT.to_string(),
        module: "".into(),
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
        name: "gleam".into(),
        package: "".to_string(),
        origin: Origin::Src,
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };

    let _ = prelude.types.insert(
        INT.to_string(),
        TypeConstructor {
            parameters: vec![],
            typ: int(),
            origin: Default::default(),
            module: "".into(),
            public: true,
        },
    );

    let _ = prelude.types_constructors.insert(
        BOOL.to_string(),
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
                constructors_count: 2,
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
                constructors_count: 2,
            },
            bool(),
        ),
    );
    let _ = prelude.types.insert(
        BOOL.to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: bool(),
            module: "".into(),
            public: true,
        },
    );

    let list_parameter = generic_var(ids.next());
    let _ = prelude.types.insert(
        LIST.to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![list_parameter.clone()],
            typ: list(list_parameter),
            module: "".into(),
            public: true,
        },
    );

    let _ = prelude.types.insert(
        FLOAT.to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: float(),
            module: "".into(),
            public: true,
        },
    );

    let _ = prelude.types.insert(
        STRING.to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: string(),
            module: "".into(),
            public: true,
        },
    );

    let result_value = generic_var(ids.next());
    let result_error = generic_var(ids.next());
    let _ = prelude.types.insert(
        RESULT.to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![result_value.clone(), result_error.clone()],
            typ: result(result_value, result_error),
            module: "".into(),
            public: true,
        },
    );

    let _ = prelude.types_constructors.insert(
        RESULT.to_string(),
        vec!["Ok".to_string(), "Error".to_string()],
    );

    let _ = prelude.values.insert(
        NIL.to_string(),
        value(
            ValueConstructorVariant::Record {
                module: "".into(),
                name: NIL.to_string(),
                arity: 0,
                field_map: None,
                location: SrcSpan::default(),
                constructors_count: 1,
            },
            nil(),
        ),
    );
    let _ = prelude.types.insert(
        NIL.to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: nil(),
            module: "".into(),
            public: true,
        },
    );

    let _ = prelude.types.insert(
        "BitString".to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: bit_string(),
            module: "".into(),
            public: true,
        },
    );

    let _ = prelude.types.insert(
        UTF_CODEPOINT.to_string(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: utf_codepoint(),
            module: "".into(),
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
                constructors_count: 2,
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
                constructors_count: 2,
            },
            fn_(vec![error.clone()], result(ok, error)),
        ),
    );

    prelude
}
