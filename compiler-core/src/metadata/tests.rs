use type_::{AccessorsMap, FieldMap, RecordAccessor};

use super::*;
use crate::{
    ast::{
        BitStringSegment, BitStringSegmentOption, CallArg, Constant, SrcSpan, TypedConstant,
        TypedConstantBitStringSegmentOption,
    },
    build::Origin,
    io::test::InMemoryFile,
    type_::{self, Module, Type, TypeConstructor, ValueConstructor, ValueConstructorVariant},
    uid::UniqueIdGenerator,
};
use std::{collections::HashMap, io::BufReader, sync::Arc};

use pretty_assertions::assert_eq;

fn roundtrip(input: &Module) -> Module {
    let buffer = InMemoryFile::new();
    ModuleEncoder::new(input).write(buffer.clone()).unwrap();
    let buffer = buffer.into_contents().unwrap();
    let ids = UniqueIdGenerator::new();
    ModuleDecoder::new(ids)
        .read(BufReader::new(buffer.as_slice()))
        .unwrap()
}

fn constant_module(constant: TypedConstant) -> Module {
    Module {
        package: "some_package".to_string(),
        origin: Origin::Src,
        name: vec!["a".to_string()],
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".to_string(),
            ValueConstructor {
                public: true,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleConstant {
                    literal: constant,
                    location: SrcSpan::default(),
                    module: "one/two".into(),
                },
            },
        )]
        .into(),
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
            type_: type_::int(),
        }],
    })
}

#[test]
fn empty_module() {
    let module = Module {
        package: "some_package".to_string(),
        origin: Origin::Src,
        name: vec!["one".to_string(), "two".to_string()],
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_app_type() {
    let module = Module {
        package: "some_package".to_string(),
        origin: Origin::Src,
        name: vec!["a".to_string(), "b".to_string()],
        types: [(
            "ListIntType".to_string(),
            TypeConstructor {
                typ: type_::list(type_::int()),
                public: true,
                origin: Default::default(),
                module: vec!["the".to_string(), "module".to_string()],
                parameters: vec![],
            },
        )]
        .into(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_fn_type() {
    let module = Module {
        package: "some_package".to_string(),
        origin: Origin::Src,
        name: vec!["a".to_string(), "b".to_string()],
        types: [(
            "FnType".to_string(),
            TypeConstructor {
                typ: type_::fn_(vec![type_::nil(), type_::float()], type_::int()),
                public: true,
                origin: Default::default(),
                module: vec!["the".to_string(), "module".to_string()],
                parameters: vec![],
            },
        )]
        .into(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_tuple_type() {
    let module = Module {
        package: "some_package".to_string(),
        origin: Origin::Src,
        name: vec!["a".to_string(), "b".to_string()],
        types: [(
            "TupleType".to_string(),
            TypeConstructor {
                typ: type_::tuple(vec![type_::nil(), type_::float(), type_::int()]),
                public: true,
                origin: Default::default(),
                module: vec!["the".to_string(), "module".to_string()],
                parameters: vec![],
            },
        )]
        .into(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_generic_type() {
    let t0 = type_::generic_var(0);
    let t1 = type_::generic_var(1);
    let t7 = type_::generic_var(7);
    let t8 = type_::generic_var(8);

    fn make(t1: Arc<Type>, t2: Arc<Type>) -> Module {
        Module {
            package: "some_package".to_string(),
            origin: Origin::Src,
            name: vec!["a".to_string(), "b".to_string()],
            types: [(
                "TupleType".to_string(),
                TypeConstructor {
                    typ: type_::tuple(vec![t1.clone(), t1.clone(), t2.clone()]),
                    public: true,
                    origin: Default::default(),
                    module: vec!["the".to_string(), "module".to_string()],
                    parameters: vec![t1, t2],
                },
            )]
            .into(),
            types_constructors: HashMap::new(),
            values: HashMap::new(),
            accessors: HashMap::new(),
        }
    }

    assert_eq!(roundtrip(&make(t7, t8)), make(t0, t1));
}

#[test]
fn module_with_type_links() {
    let linked_type = type_::link(type_::int());
    let type_ = type_::int();

    fn make(type_: Arc<Type>) -> Module {
        Module {
            package: "some_package".to_string(),
            origin: Origin::Src,
            name: vec!["a".to_string()],
            types: [(
                "SomeType".to_string(),
                TypeConstructor {
                    typ: type_,
                    public: true,
                    origin: Default::default(),
                    module: vec!["a".to_string()],
                    parameters: vec![],
                },
            )]
            .into(),
            types_constructors: HashMap::new(),
            values: HashMap::new(),
            accessors: HashMap::new(),
        }
    }

    assert_eq!(roundtrip(&make(linked_type)), make(type_));
}

#[test]
fn module_type_to_constructors_mapping() {
    let module = Module {
        package: "some_package".to_string(),
        origin: Origin::Src,
        name: vec!["a".to_string()],
        types: HashMap::new(),
        types_constructors: [("SomeType".to_string(), vec!["One".to_string()])].into(),
        accessors: HashMap::new(),
        values: HashMap::new(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_fn_value() {
    let module = Module {
        package: "some_package".to_string(),
        origin: Origin::Src,
        name: vec!["a".to_string()],
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".to_string(),
            ValueConstructor {
                public: true,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    name: "one".to_string(),
                    field_map: None,
                    module: vec!["a".to_string()],
                    arity: 5,
                    location: SrcSpan {
                        start: 535,
                        end: 1100,
                    },
                },
            },
        )]
        .into(),
    };

    assert_eq!(roundtrip(&module), module);
}

// https://github.com/gleam-lang/gleam/commit/c8f3bd0ddbf61c27ea35f37297058ecca7515f6c
#[test]
fn module_fn_value_regression() {
    let module = Module {
        package: "some_package".to_string(),
        origin: Origin::Src,
        name: vec!["a".into(), "b".into(), "c".into()],
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".to_string(),
            ValueConstructor {
                public: true,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    name: "one".to_string(),
                    field_map: None,
                    module: vec!["a".to_string()],
                    arity: 5,
                    location: SrcSpan {
                        start: 52,
                        end: 1100,
                    },
                },
            },
        )]
        .into(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_fn_value_with_field_map() {
    let module = Module {
        package: "some_package".to_string(),
        origin: Origin::Src,
        name: vec!["a".to_string()],
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".to_string(),
            ValueConstructor {
                public: true,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    name: "one".to_string(),
                    field_map: Some(FieldMap {
                        arity: 20,
                        fields: [("ok".to_string(), 5), ("ko".to_string(), 7)].into(),
                    }),
                    module: vec!["a".to_string()],
                    arity: 5,
                    location: SrcSpan { start: 2, end: 11 },
                },
            },
        )]
        .into(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn record_value() {
    let module = Module {
        package: "some_package".to_string(),
        origin: Origin::Src,
        name: vec!["a".to_string()],
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".to_string(),
            ValueConstructor {
                public: true,
                type_: type_::int(),
                variant: ValueConstructorVariant::Record {
                    name: "one".to_string(),
                    module: "themodule".to_string(),
                    field_map: None,
                    arity: 5,
                    location: SrcSpan {
                        start: 144,
                        end: 155,
                    },
                },
            },
        )]
        .into(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn record_value_with_field_map() {
    let module = Module {
        package: "some_package".to_string(),
        origin: Origin::Src,
        name: vec!["a".to_string()],
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".to_string(),
            ValueConstructor {
                public: true,
                type_: type_::int(),
                variant: ValueConstructorVariant::Record {
                    module: "themodule".to_string(),
                    name: "one".to_string(),
                    field_map: Some(FieldMap {
                        arity: 20,
                        fields: [("ok".to_string(), 5), ("ko".to_string(), 7)].into(),
                    }),
                    arity: 5,
                    location: SrcSpan { start: 5, end: 11 },
                },
            },
        )]
        .into(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn accessors() {
    let module = Module {
        package: "some_package".to_string(),
        origin: Origin::Src,
        name: vec!["a".to_string()],
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: [
            (
                "one".to_string(),
                AccessorsMap {
                    public: true,
                    type_: type_::int(),
                    accessors: [
                        (
                            "a".to_string(),
                            RecordAccessor {
                                index: 6,
                                label: "siiixxx".to_string(),
                                type_: type_::nil(),
                            },
                        ),
                        (
                            "a".to_string(),
                            RecordAccessor {
                                index: 5,
                                label: "fiveee".to_string(),
                                type_: type_::float(),
                            },
                        ),
                    ]
                    .into(),
                },
            ),
            (
                "two".to_string(),
                AccessorsMap {
                    public: true,
                    type_: type_::int(),
                    accessors: [(
                        "a".to_string(),
                        RecordAccessor {
                            index: 1,
                            label: "ok".to_string(),
                            type_: type_::float(),
                        },
                    )]
                    .into(),
                },
            ),
        ]
        .into(),
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
        typ: type_::int(),
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
        typ: type_::int(),
        field_map: None,
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
        value: 234,
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
    let module = bit_string_segment_option_module(BitStringSegmentOption::Int {
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

#[test]
fn constant_bit_string_bit_string() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::BitString {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_utf8() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Utf8 {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_utf16() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Utf16 {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_utf32() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Utf32 {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_utf8codepoint() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Utf8Codepoint {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_utf16codepoint() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Utf16Codepoint {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_utf32codepoint() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Utf32Codepoint {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_signed() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Signed {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_unsigned() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Unsigned {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_big() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Big {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_little() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Little {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_string_native() {
    let module = bit_string_segment_option_module(BitStringSegmentOption::Native {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}
