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
        name: INT.into(),
        module: "".into(),
        args: vec![],
    })
}

pub fn float() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: FLOAT.into(),
        module: "".into(),
    })
}

pub fn bool() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: BOOL.into(),
        module: "".into(),
    })
}

pub fn string() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: STRING.into(),
        module: "".into(),
    })
}

pub fn nil() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: NIL.into(),
        module: "".into(),
    })
}

pub fn list(t: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: LIST.into(),
        module: "".into(),
        args: vec![t],
    })
}

pub fn result(a: Arc<Type>, e: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: RESULT.into(),
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
        name: BIT_STRING.into(),
        module: "".into(),
    })
}

pub fn utf_codepoint() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: UTF_CODEPOINT.into(),
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
        package: "".into(),
        origin: Origin::Src,
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };

    let _ = prelude.types.insert(
        INT.into(),
        TypeConstructor {
            parameters: vec![],
            typ: int(),
            origin: Default::default(),
            module: "".into(),
            public: true,
        },
    );

    let _ = prelude
        .types_constructors
        .insert(BOOL.into(), vec!["True".into(), "False".into()]);

    let _ = prelude.values.insert(
        "True".into(),
        value(
            ValueConstructorVariant::Record {
                documentation: None,
                module: "".into(),
                name: "True".into(),
                field_map: None,
                arity: 0,
                location: SrcSpan::default(),
                constructors_count: 2,
            },
            bool(),
        ),
    );
    let _ = prelude.values.insert(
        "False".into(),
        value(
            ValueConstructorVariant::Record {
                documentation: None,
                module: "".into(),
                name: "False".into(),
                field_map: None,
                arity: 0,
                location: SrcSpan::default(),
                constructors_count: 2,
            },
            bool(),
        ),
    );
    let _ = prelude.types.insert(
        BOOL.into(),
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
        LIST.into(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![list_parameter.clone()],
            typ: list(list_parameter),
            module: "".into(),
            public: true,
        },
    );

    let _ = prelude.types.insert(
        FLOAT.into(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: float(),
            module: "".into(),
            public: true,
        },
    );

    let _ = prelude.types.insert(
        STRING.into(),
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
        RESULT.into(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![result_value.clone(), result_error.clone()],
            typ: result(result_value, result_error),
            module: "".into(),
            public: true,
        },
    );

    let _ = prelude
        .types_constructors
        .insert(RESULT.into(), vec!["Ok".into(), "Error".into()]);

    let _ = prelude.values.insert(
        NIL.into(),
        value(
            ValueConstructorVariant::Record {
                documentation: None,
                module: "".into(),
                name: NIL.into(),
                arity: 0,
                field_map: None,
                location: SrcSpan::default(),
                constructors_count: 1,
            },
            nil(),
        ),
    );
    let _ = prelude.types.insert(
        NIL.into(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: nil(),
            module: "".into(),
            public: true,
        },
    );

    let _ = prelude.types.insert(
        "BitString".into(),
        TypeConstructor {
            origin: Default::default(),
            parameters: vec![],
            typ: bit_string(),
            module: "".into(),
            public: true,
        },
    );

    let _ = prelude.types.insert(
        UTF_CODEPOINT.into(),
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
        "Ok".into(),
        value(
            ValueConstructorVariant::Record {
                documentation: None,
                module: "".into(),
                name: "Ok".into(),
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
        "Error".into(),
        value(
            ValueConstructorVariant::Record {
                documentation: None,
                module: "".into(),
                name: "Error".into(),
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
