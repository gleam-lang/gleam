use super::{Environment, Type, TypeConstructor, ValueConstructorVariant};
use crate::error::GleamExpect;
use std::sync::Arc;

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

pub fn register_prelude<'a, 'b>(mut typer: Environment<'a, 'b>) -> Environment<'a, 'b> {
    typer
        .insert_type_constructor(
            "Int".to_string(),
            TypeConstructor {
                parameters: vec![],
                typ: int(),
                origin: Default::default(),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting Int type");

    typer.insert_variable(
        "True".to_string(),
        ValueConstructorVariant::Record {
            name: "True".to_string(),
            field_map: None,
        },
        bool(),
    );
    typer.insert_variable(
        "False".to_string(),
        ValueConstructorVariant::Record {
            name: "False".to_string(),
            field_map: None,
        },
        bool(),
    );
    typer
        .insert_type_constructor(
            "Bool".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![],
                typ: bool(),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting Bool type");

    let list_parameter = typer.new_generic_var();
    typer
        .insert_type_constructor(
            "List".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![list_parameter.clone()],
                typ: list(list_parameter),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting List type");

    typer
        .insert_type_constructor(
            "Float".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![],
                typ: float(),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting Float type");

    typer
        .insert_type_constructor(
            "String".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![],
                typ: string(),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting String type");

    let result_value = typer.new_generic_var();
    let result_error = typer.new_generic_var();
    typer
        .insert_type_constructor(
            "Result".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![result_value.clone(), result_error.clone()],
                typ: result(result_value, result_error),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting Result type");

    typer.insert_variable(
        "Nil".to_string(),
        ValueConstructorVariant::Record {
            name: "Nil".to_string(),
            field_map: None,
        },
        nil(),
    );
    typer
        .insert_type_constructor(
            "Nil".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![],
                typ: nil(),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting Nil type");

    typer
        .insert_type_constructor(
            "BitString".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![],
                typ: bit_string(),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting BitString type");

    typer
        .insert_type_constructor(
            "UtfCodepoint".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![],
                typ: utf_codepoint(),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting UTF Codepoint type");

    let ok = typer.new_generic_var();
    let error = typer.new_generic_var();
    typer.insert_variable(
        "Ok".to_string(),
        ValueConstructorVariant::Record {
            name: "Ok".to_string(),
            field_map: None,
        },
        fn_(vec![ok.clone()], result(ok, error)),
    );

    let ok = typer.new_generic_var();
    let error = typer.new_generic_var();
    typer.insert_variable(
        "Error".to_string(),
        ValueConstructorVariant::Record {
            name: "Error".to_string(),
            field_map: None,
        },
        fn_(vec![error.clone()], result(ok, error)),
    );

    typer
}
