use rand::Rng;
use type_::{AccessorsMap, FieldMap, RecordAccessor};

use super::*;
use crate::{
    ast::{
        BitStringSegment, BitStringSegmentOption, CallArg, Constant, SrcSpan, TypedConstant,
        TypedConstantBitStringSegmentOption,
    },
    build::Origin,
    type_::{self, Module, Type, TypeConstructor, ValueConstructor, ValueConstructorVariant},
};
use std::{collections::HashMap, io::BufReader, sync::Arc};

use pretty_assertions::assert_eq;

fn roundtrip(input: &Module) -> Module {
    let buffer = Metadata::encode(input).unwrap();
    Metadata::decode(UniqueIdGenerator::new(), BufReader::new(buffer.as_slice())).unwrap()
}

fn constant_module(constant: TypedConstant) -> Module {
    Module {
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
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
                value: "1".into(),
            }),
            options: vec![option],
            type_: type_::int(),
        }],
    })
}

#[test]
fn empty_module() {
    let module = Module {
        package: "some_package".into(),
        origin: Origin::Src,
        name: "one/two".into(),
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
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b".into(),
        types: [(
            "ListIntType".into(),
            TypeConstructor {
                typ: type_::list(type_::int()),
                public: true,
                origin: Default::default(),
                module: "the/module".into(),
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
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b".into(),
        types: [(
            "FnType".into(),
            TypeConstructor {
                typ: type_::fn_(vec![type_::nil(), type_::float()], type_::int()),
                public: true,
                origin: Default::default(),
                module: "the/module".into(),
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
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b".into(),
        types: [(
            "TupleType".into(),
            TypeConstructor {
                typ: type_::tuple(vec![type_::nil(), type_::float(), type_::int()]),
                public: true,
                origin: Default::default(),
                module: "the/module".into(),
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
            package: "some_package".into(),
            origin: Origin::Src,
            name: "a/b".into(),
            types: [(
                "TupleType".into(),
                TypeConstructor {
                    typ: type_::tuple(vec![t1.clone(), t1.clone(), t2.clone()]),
                    public: true,
                    origin: Default::default(),
                    module: "the/module".into(),
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
            package: "some_package".into(),
            origin: Origin::Src,
            name: "a".into(),
            types: [(
                "SomeType".into(),
                TypeConstructor {
                    typ: type_,
                    public: true,
                    origin: Default::default(),
                    module: "a".into(),
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
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_constructors: [("SomeType".into(), vec!["One".into()])].into(),
        accessors: HashMap::new(),
        values: HashMap::new(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_fn_value() {
    let module = Module {
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                public: true,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    name: "one".into(),
                    field_map: None,
                    module: "a".into(),
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
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b/c".into(),
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                public: true,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    name: "one".into(),
                    field_map: None,
                    module: "a".into(),
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
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                public: true,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    name: "one".into(),
                    field_map: Some(FieldMap {
                        arity: 20,
                        fields: [("ok".into(), 5), ("ko".into(), 7)].into(),
                    }),
                    module: "a".into(),
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
    let mut random = rand::thread_rng();

    let module = Module {
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                public: true,
                type_: type_::int(),
                variant: ValueConstructorVariant::Record {
                    name: "one".into(),
                    module: "themodule".into(),
                    field_map: None,
                    arity: random.gen(),
                    constructors_count: random.gen(),
                    location: SrcSpan {
                        start: random.gen(),
                        end: random.gen(),
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
    let mut random = rand::thread_rng();

    let module = Module {
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                public: true,
                type_: type_::int(),
                variant: ValueConstructorVariant::Record {
                    module: "themodule".into(),
                    name: "one".into(),
                    field_map: Some(FieldMap {
                        arity: random.gen(),
                        fields: [("ok".into(), random.gen()), ("ko".into(), random.gen())].into(),
                    }),
                    arity: random.gen(),
                    constructors_count: random.gen(),
                    location: SrcSpan {
                        start: random.gen(),
                        end: random.gen(),
                    },
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
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: [
            (
                "one".into(),
                AccessorsMap {
                    public: true,
                    type_: type_::int(),
                    accessors: [
                        (
                            "a".into(),
                            RecordAccessor {
                                index: 6,
                                label: "siiixxx".into(),
                                type_: type_::nil(),
                            },
                        ),
                        (
                            "a".into(),
                            RecordAccessor {
                                index: 5,
                                label: "fiveee".into(),
                                type_: type_::float(),
                            },
                        ),
                    ]
                    .into(),
                },
            ),
            (
                "two".into(),
                AccessorsMap {
                    public: true,
                    type_: type_::int(),
                    accessors: [(
                        "a".into(),
                        RecordAccessor {
                            index: 1,
                            label: "ok".into(),
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
        value: "100".into(),
    });

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_float() {
    let module = constant_module(Constant::Float {
        location: Default::default(),
        value: "1.0".into(),
    });

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_string() {
    let module = constant_module(Constant::String {
        location: Default::default(),
        value: "hello".into(),
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
                value: "1".into(),
            },
            Constant::Float {
                location: Default::default(),
                value: "1.0".into(),
            },
            Constant::Tuple {
                location: Default::default(),
                elements: vec![
                    Constant::Int {
                        location: Default::default(),
                        value: "1".into(),
                    },
                    Constant::Float {
                        location: Default::default(),
                        value: "1.0".into(),
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
                value: "1".into(),
            },
            Constant::Int {
                location: Default::default(),
                value: "2".into(),
            },
            Constant::Int {
                location: Default::default(),
                value: "3".into(),
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
        name: "".into(),
        args: vec![
            CallArg {
                implicit: false,
                label: None,
                location: Default::default(),
                value: Constant::Float {
                    location: Default::default(),
                    value: "0.0".into(),
                },
            },
            CallArg {
                implicit: false,
                label: None,
                location: Default::default(),
                value: Constant::Int {
                    location: Default::default(),
                    value: "1".into(),
                },
            },
        ],
        tag: "thetag".into(),
        typ: type_::int(),
        field_map: None,
    });

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_var() {
    let one_original = Constant::Int {
        location: Default::default(),
        value: "1".into(),
    };

    let one = Constant::Var {
        location: Default::default(),
        module: None,
        name: "one_original".into(),
        typ: type_::int(),
        constructor: Some(Box::from(ValueConstructor {
            public: true,
            type_: type_::int(),
            variant: ValueConstructorVariant::ModuleConstant {
                literal: one_original.clone(),
                location: SrcSpan::default(),
                module: "one/two".into(),
            },
        })),
    };

    let module = Module {
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [
            (
                "one".into(),
                ValueConstructor {
                    public: true,
                    type_: type_::int(),
                    variant: ValueConstructorVariant::ModuleConstant {
                        literal: one,
                        location: SrcSpan::default(),
                        module: "one/two".into(),
                    },
                },
            ),
            (
                "one_original".into(),
                ValueConstructor {
                    public: true,
                    type_: type_::int(),
                    variant: ValueConstructorVariant::ModuleConstant {
                        literal: one_original,
                        location: SrcSpan::default(),
                        module: "one/two".into(),
                    },
                },
            ),
        ]
        .into(),
    };

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
            value: "1".into(),
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
            value: "1".into(),
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
