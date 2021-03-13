use clap::value_t;

use super::*;
use crate::{
    fs::test::InMemoryFile,
    typ::{self, TypeVar},
};
use std::{io::BufReader, iter::FromIterator};

fn roundtrip(input: &Module) -> Module {
    let mut buffer = InMemoryFile::new();
    ModuleEncoder::new(input).write(buffer.clone()).unwrap();
    let buffer = buffer.into_contents().unwrap();
    ModuleDecoder::new()
        .read(BufReader::new(buffer.as_slice()))
        .unwrap()
}

fn constant_module(constant: TypedConstant) -> Module {
    Module {
        name: vec!["a".to_string()],
        types: HashMap::new(),
        accessors: HashMap::new(),
        values: vec![(
            "one".to_string(),
            ValueConstructor {
                public: true,
                origin: Default::default(),
                type_: typ::int(),
                variant: ValueConstructorVariant::ModuleConstant { literal: constant },
            },
        )]
        .into_iter()
        .collect(),
    }
}

fn bit_string_segment_option_module(option: TypedConstantBitStringSegmentOption) -> Module {
    constant_module(Constant::BitString {
        location: Default::default(),
        segments: vec![BitStringSegment {
            location: Default::default(),
            value: Box::new(Constant::Int {
                location: Default::default(),
                value: "1".to_string(),
            }),
            options: vec![option],
            type_: typ::int(),
        }],
    })
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

#[test]
fn constant_int() {
    let module = constant_module(Constant::Int {
        location: Default::default(),
        value: "100".to_string(),
    });

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_float() {
    let module = constant_module(Constant::Float {
        location: Default::default(),
        value: "1.0".to_string(),
    });

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_string() {
    let module = constant_module(Constant::String {
        location: Default::default(),
        value: "hello".to_string(),
    });

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_tuple() {
    let module = constant_module(Constant::Tuple {
        location: Default::default(),
        elements: vec![
            Constant::Int {
                location: Default::default(),
                value: "1".to_string(),
            },
            Constant::Float {
                location: Default::default(),
                value: "1.0".to_string(),
            },
            Constant::Tuple {
                location: Default::default(),
                elements: vec![
                    Constant::Int {
                        location: Default::default(),
                        value: "1".to_string(),
                    },
                    Constant::Float {
                        location: Default::default(),
                        value: "1.0".to_string(),
                    },
                ],
            },
        ],
    });

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_list() {
    let module = constant_module(Constant::List {
        location: Default::default(),
        typ: typ::int(),
        elements: vec![
            Constant::Int {
                location: Default::default(),
                value: "1".to_string(),
            },
            Constant::Int {
                location: Default::default(),
                value: "2".to_string(),
            },
            Constant::Int {
                location: Default::default(),
                value: "3".to_string(),
            },
        ],
    });

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_record() {
    let module = constant_module(Constant::Record {
        location: Default::default(),
        module: None,
        name: "".to_string(),
        args: vec![
            CallArg {
                label: None,
                location: Default::default(),
                value: Constant::Float {
                    location: Default::default(),
                    value: "0.0".to_string(),
                },
            },
            CallArg {
                label: None,
                location: Default::default(),
                value: Constant::Int {
                    location: Default::default(),
                    value: "1".to_string(),
                },
            },
        ],
        tag: "thetag".to_string(),
        typ: typ::int(),
    });

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string() {
    let module = constant_module(Constant::BitString {
        location: Default::default(),
        segments: vec![],
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_unit() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Unit {
        location: Default::default(),
        value: Box::new(1),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_float() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Float {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_int() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Integer {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_size() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Size {
        location: Default::default(),
        value: Box::new(Constant::Int {
            location: Default::default(),
            value: "1".to_string(),
        }),
        short_form: false,
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_size_short_form() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Size {
        location: Default::default(),
        value: Box::new(Constant::Int {
            location: Default::default(),
            value: "1".to_string(),
        }),
        short_form: true,
    });
    assert_eq!(roundtrip(&module), module);
}

// Which::Bitstring(reader) => todo!(),
// Which::Utf8(reader) => todo!(),
// Which::Utf16(reader) => todo!(),
// Which::Utf32(reader) => todo!(),
// Which::Utf8Codepoint(reader) => todo!(),
// Which::Utf16Codepoint(reader) => todo!(),
// Which::Utf32Codepoint(reader) => todo!(),
// Which::Signed(reader) => todo!(),
// Which::Unsigned(reader) => todo!(),
// Which::Big(reader) => todo!(),
// Which::Little(reader) => todo!(),
// Which::Native(reader) => todo!(),
// Which::Unit(reader) => todo!(),
