use super::*;
use crate::{
    fs::test::InMemoryFile,
    typ::{self, TypeVar},
};
use std::{io::BufReader, iter::FromIterator};

// TODO: test type links

fn roundtrip(input: &Module) -> Module {
    let mut buffer = InMemoryFile::new();
    ModuleEncoder::new(input).write(buffer.clone()).unwrap();
    let buffer = buffer.into_contents().unwrap();
    ModuleDecoder::new()
        .read(BufReader::new(buffer.as_slice()))
        .unwrap()
}

#[test]
fn empty_module() {
    let module = Module {
        name: vec!["one".to_string(), "two".to_string()],
        types: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_app_type() {
    let module = Module {
        name: vec!["a".to_string(), "b".to_string()],
        types: vec![(
            "ListIntType".to_string(),
            TypeConstructor {
                typ: typ::list(typ::int()),
                public: true,
                origin: Default::default(),
                module: vec!["the".to_string(), "module".to_string()],
                parameters: vec![],
            },
        )]
        .into_iter()
        .collect(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_fn_type() {
    let module = Module {
        name: vec!["a".to_string(), "b".to_string()],
        types: vec![(
            "FnType".to_string(),
            TypeConstructor {
                typ: typ::fn_(vec![typ::nil(), typ::float()], typ::int()),
                public: true,
                origin: Default::default(),
                module: vec!["the".to_string(), "module".to_string()],
                parameters: vec![],
            },
        )]
        .into_iter()
        .collect(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_tuple_type() {
    let module = Module {
        name: vec!["a".to_string(), "b".to_string()],
        types: vec![(
            "TupleType".to_string(),
            TypeConstructor {
                typ: typ::tuple(vec![typ::nil(), typ::float(), typ::int()]),
                public: true,
                origin: Default::default(),
                module: vec!["the".to_string(), "module".to_string()],
                parameters: vec![],
            },
        )]
        .into_iter()
        .collect(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_generic_type() {
    let t0 = typ::generic_var(0);
    let t1 = typ::generic_var(1);
    let t7 = typ::generic_var(7);
    let t8 = typ::generic_var(8);

    fn make(t1: Arc<Type>, t2: Arc<Type>) -> Module {
        Module {
            name: vec!["a".to_string(), "b".to_string()],
            types: vec![(
                "TupleType".to_string(),
                TypeConstructor {
                    typ: typ::tuple(vec![t1.clone(), t1.clone(), t2.clone()]),
                    public: true,
                    origin: Default::default(),
                    module: vec!["the".to_string(), "module".to_string()],
                    parameters: vec![t1, t2],
                },
            )]
            .into_iter()
            .collect(),
            values: HashMap::new(),
            accessors: HashMap::new(),
        }
    }

    assert_eq!(roundtrip(&make(t7, t8)), make(t0, t1));
}

#[test]
fn module_with_type_links() {
    let linked_type = typ::link(typ::int());
    let type_ = typ::int();

    fn make(type_: Arc<Type>) -> Module {
        Module {
            name: vec!["a".to_string()],
            types: vec![(
                "SomeType".to_string(),
                TypeConstructor {
                    typ: type_,
                    public: true,
                    origin: Default::default(),
                    module: vec!["a".to_string()],
                    parameters: vec![],
                },
            )]
            .into_iter()
            .collect(),
            values: HashMap::new(),
            accessors: HashMap::new(),
        }
    }

    assert_eq!(roundtrip(&make(linked_type)), make(type_));
}

#[test]
fn module_fn_value() {
    let module = Module {
        name: vec!["a".to_string()],
        types: HashMap::new(),
        accessors: HashMap::new(),
        values: vec![(
            "one".to_string(),
            ValueConstructor {
                public: true,
                origin: Default::default(),
                type_: typ::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    name: "one".to_string(),
                    field_map: None,
                    module: vec!["a".to_string()],
                    arity: 5,
                },
            },
        )]
        .into_iter()
        .collect(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_fn_value_with_field_map() {
    let module = Module {
        name: vec!["a".to_string()],
        types: HashMap::new(),
        accessors: HashMap::new(),
        values: vec![(
            "one".to_string(),
            ValueConstructor {
                public: true,
                origin: Default::default(),
                type_: typ::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    name: "one".to_string(),
                    field_map: Some(FieldMap {
                        arity: 20,
                        fields: vec![("ok".to_string(), 5), ("ko".to_string(), 7)]
                            .into_iter()
                            .collect(),
                    }),
                    module: vec!["a".to_string()],
                    arity: 5,
                },
            },
        )]
        .into_iter()
        .collect(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn record_value() {
    let module = Module {
        name: vec!["a".to_string()],
        types: HashMap::new(),
        accessors: HashMap::new(),
        values: vec![(
            "one".to_string(),
            ValueConstructor {
                public: true,
                origin: Default::default(),
                type_: typ::int(),
                variant: ValueConstructorVariant::Record {
                    name: "one".to_string(),
                    field_map: None,
                    arity: 5,
                },
            },
        )]
        .into_iter()
        .collect(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn record_value_with_field_map() {
    let module = Module {
        name: vec!["a".to_string()],
        types: HashMap::new(),
        accessors: HashMap::new(),
        values: vec![(
            "one".to_string(),
            ValueConstructor {
                public: true,
                origin: Default::default(),
                type_: typ::int(),
                variant: ValueConstructorVariant::Record {
                    name: "one".to_string(),
                    field_map: Some(FieldMap {
                        arity: 20,
                        fields: vec![("ok".to_string(), 5), ("ko".to_string(), 7)]
                            .into_iter()
                            .collect(),
                    }),
                    arity: 5,
                },
            },
        )]
        .into_iter()
        .collect(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn accessors() {
    let module = Module {
        name: vec!["a".to_string()],
        types: HashMap::new(),
        values: HashMap::new(),
        accessors: vec![
            (
                "one".to_string(),
                AccessorsMap {
                    public: true,
                    type_: typ::int(),
                    accessors: vec![
                        (
                            "a".to_string(),
                            RecordAccessor {
                                index: 6,
                                label: "siiixxx".to_string(),
                                type_: typ::nil(),
                            },
                        ),
                        (
                            "a".to_string(),
                            RecordAccessor {
                                index: 5,
                                label: "fiveee".to_string(),
                                type_: typ::float(),
                            },
                        ),
                    ]
                    .into_iter()
                    .collect(),
                },
            ),
            (
                "two".to_string(),
                AccessorsMap {
                    public: true,
                    type_: typ::int(),
                    accessors: vec![(
                        "a".to_string(),
                        RecordAccessor {
                            index: 1,
                            label: "ok".to_string(),
                            type_: typ::float(),
                        },
                    )]
                    .into_iter()
                    .collect(),
                },
            ),
        ]
        .into_iter()
        .collect(),
    };

    assert_eq!(roundtrip(&module), module);
}
