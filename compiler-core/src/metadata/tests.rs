use hexpm::version::Version;
use rand::Rng;
use type_::{AccessorsMap, FieldMap, RecordAccessor};

use super::*;
use crate::{
    ast::{
        BitArrayOption, BitArraySegment, CallArg, Constant, Publicity, SrcSpan, TypedConstant,
        TypedConstantBitArraySegmentOption,
    },
    build::Origin,
    line_numbers::LineNumbers,
    reference::{Reference, ReferenceKind},
    type_::{
        self, Deprecation, ModuleInterface, Opaque, References, Type, TypeAliasConstructor,
        TypeConstructor, TypeValueConstructor, TypeValueConstructorField, TypeVariantConstructors,
        ValueConstructor, ValueConstructorVariant,
        expression::{Implementations, Purity},
        prelude,
    },
    uid::UniqueIdGenerator,
};
use std::{collections::HashMap, io::BufReader, sync::Arc};

use pretty_assertions::assert_eq;

fn roundtrip(input: &ModuleInterface) -> ModuleInterface {
    let buffer = ModuleEncoder::new(input).encode().unwrap();
    let ids = UniqueIdGenerator::new();
    ModuleDecoder::new(ids)
        .read(BufReader::new(buffer.as_slice()))
        .unwrap()
}

fn constant_module(constant: TypedConstant) -> ModuleInterface {
    ModuleInterface {
        warnings: vec![],
        is_internal: true,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                publicity: Publicity::Public,
                deprecation: Deprecation::NotDeprecated,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleConstant {
                    documentation: Some("Some documentation".into()),
                    literal: constant,
                    location: SrcSpan::default(),
                    module: "one/two".into(),
                    implementations: Implementations {
                        gleam: true,
                        uses_erlang_externals: false,
                        uses_javascript_externals: false,
                        can_run_on_erlang: true,
                        can_run_on_javascript: true,
                    },
                    name: "one".into(),
                },
            },
        )]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    }
}

fn bit_array_segment_option_module(option: TypedConstantBitArraySegmentOption) -> ModuleInterface {
    constant_module(Constant::BitArray {
        location: Default::default(),
        segments: vec![BitArraySegment {
            location: Default::default(),
            value: Box::new(Constant::Int {
                location: Default::default(),
                value: "1".into(),
                int_value: 1.into(),
            }),
            options: vec![option],
            type_: type_::int(),
        }],
    })
}

#[test]
fn empty_module() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: true,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "one/two".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn with_line_numbers() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "one/two".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        line_numbers: LineNumbers::new(
            "const a = 1
        const b = 2
        const c = 3",
        ),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_private_type() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b".into(),
        types: [(
            "ListIntType".into(),
            TypeConstructor {
                type_: type_::list(type_::int()),
                publicity: Publicity::Private,
                origin: Default::default(),
                module: "the/module".into(),
                parameters: vec![],
                deprecation: Deprecation::NotDeprecated,
                documentation: None,
            },
        )]
        .into(),
        types_value_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_app_type() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b".into(),
        types: [(
            "ListIntType".into(),
            TypeConstructor {
                type_: type_::list(type_::int()),
                publicity: Publicity::Public,
                origin: Default::default(),
                module: "the/module".into(),
                parameters: vec![],
                deprecation: Deprecation::NotDeprecated,
                documentation: None,
            },
        )]
        .into(),
        types_value_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_fn_type() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b".into(),
        types: [(
            "FnType".into(),
            TypeConstructor {
                type_: type_::fn_(vec![type_::nil(), type_::float()], type_::int()),
                publicity: Publicity::Public,
                origin: Default::default(),
                module: "the/module".into(),
                parameters: vec![],
                deprecation: Deprecation::NotDeprecated,
                documentation: None,
            },
        )]
        .into(),
        types_value_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_tuple_type() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b".into(),
        types: [(
            "TupleType".into(),
            TypeConstructor {
                type_: type_::tuple(vec![type_::nil(), type_::float(), type_::int()]),
                publicity: Publicity::Public,
                origin: Default::default(),
                module: "the/module".into(),
                parameters: vec![],
                deprecation: Deprecation::NotDeprecated,
                documentation: None,
            },
        )]
        .into(),
        types_value_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_generic_type() {
    let t0 = type_::generic_var(0);
    let t1 = type_::generic_var(1);
    let t7 = type_::generic_var(7);
    let t8 = type_::generic_var(8);

    fn make(t1: Arc<Type>, t2: Arc<Type>) -> ModuleInterface {
        ModuleInterface {
            warnings: vec![],
            is_internal: false,
            package: "some_package".into(),
            origin: Origin::Src,
            name: "a/b".into(),
            types: [(
                "TupleType".into(),
                TypeConstructor {
                    type_: type_::tuple(vec![t1.clone(), t1.clone(), t2.clone()]),
                    publicity: Publicity::Public,
                    origin: Default::default(),
                    module: "the/module".into(),
                    parameters: vec![t1, t2],
                    deprecation: Deprecation::NotDeprecated,
                    documentation: None,
                },
            )]
            .into(),
            types_value_constructors: HashMap::new(),
            values: HashMap::new(),
            accessors: HashMap::new(),
            line_numbers: LineNumbers::new(""),
            src_path: "some_path".into(),
            minimum_required_version: Version::new(0, 1, 0),
            type_aliases: HashMap::new(),
            documentation: Vec::new(),
            contains_echo: false,
            references: References::default(),
        }
    }

    assert_eq!(roundtrip(&make(t7, t8)), make(t0, t1));
}

#[test]
fn module_with_type_links() {
    let linked_type = type_::link(type_::int());
    let type_ = type_::int();

    fn make(type_: Arc<Type>) -> ModuleInterface {
        ModuleInterface {
            warnings: vec![],
            is_internal: false,
            package: "some_package".into(),
            origin: Origin::Src,
            name: "a".into(),
            types: [(
                "SomeType".into(),
                TypeConstructor {
                    type_,
                    publicity: Publicity::Public,
                    origin: Default::default(),
                    module: "a".into(),
                    parameters: vec![],
                    deprecation: Deprecation::NotDeprecated,
                    documentation: None,
                },
            )]
            .into(),
            types_value_constructors: HashMap::new(),
            values: HashMap::new(),
            accessors: HashMap::new(),
            line_numbers: LineNumbers::new(""),
            src_path: "some_path".into(),
            minimum_required_version: Version::new(0, 1, 0),
            type_aliases: HashMap::new(),
            documentation: Vec::new(),
            contains_echo: false,
            references: References::default(),
        }
    }

    assert_eq!(roundtrip(&make(linked_type)), make(type_));
}

#[test]
fn module_with_type_constructor_documentation() {
    let linked_type = type_::link(type_::int());
    let type_ = type_::int();

    fn make(type_: Arc<Type>) -> ModuleInterface {
        ModuleInterface {
            warnings: vec![],
            is_internal: false,
            package: "some_package".into(),
            origin: Origin::Src,
            name: "a".into(),
            types: [(
                "SomeType".into(),
                TypeConstructor {
                    type_,
                    publicity: Publicity::Public,
                    origin: Default::default(),
                    module: "a".into(),
                    parameters: vec![],
                    deprecation: Deprecation::NotDeprecated,
                    documentation: Some("type documentation".into()),
                },
            )]
            .into(),
            types_value_constructors: HashMap::new(),
            values: HashMap::new(),
            accessors: HashMap::new(),
            line_numbers: LineNumbers::new(""),
            src_path: "some_path".into(),
            minimum_required_version: Version::new(0, 1, 0),
            type_aliases: HashMap::new(),
            documentation: Vec::new(),
            contains_echo: false,
            references: References::default(),
        }
    }

    assert_eq!(roundtrip(&make(linked_type)), make(type_));
}

#[test]
fn module_with_type_constructor_origin() {
    let linked_type = type_::link(type_::int());
    let type_ = type_::int();

    fn make(type_: Arc<Type>) -> ModuleInterface {
        ModuleInterface {
            warnings: vec![],
            is_internal: false,
            package: "some_package".into(),
            origin: Origin::Src,
            name: "a".into(),
            types: [(
                "SomeType".into(),
                TypeConstructor {
                    type_,
                    publicity: Publicity::Public,
                    origin: SrcSpan {
                        start: 535,
                        end: 543,
                    },
                    module: "a".into(),
                    parameters: vec![],
                    deprecation: Deprecation::NotDeprecated,
                    documentation: None,
                },
            )]
            .into(),
            types_value_constructors: HashMap::new(),
            values: HashMap::new(),
            accessors: HashMap::new(),
            line_numbers: LineNumbers::new(""),
            src_path: "some_path".into(),
            minimum_required_version: Version::new(0, 1, 0),
            type_aliases: HashMap::new(),
            documentation: Vec::new(),
            contains_echo: false,
            references: References::default(),
        }
    }

    assert_eq!(roundtrip(&make(linked_type)), make(type_));
}

#[test]
fn module_type_to_constructors_mapping() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: [(
            "SomeType".into(),
            TypeVariantConstructors {
                type_parameters_ids: vec![0, 1, 2],
                variants: vec![TypeValueConstructor {
                    name: "One".into(),
                    parameters: vec![],
                    documentation: Some("Some documentation".into()),
                }],
                opaque: Opaque::NotOpaque,
            },
        )]
        .into(),
        accessors: HashMap::new(),
        values: HashMap::new(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_fn_value() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                publicity: Publicity::Public,
                deprecation: Deprecation::NotDeprecated,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    documentation: Some("wobble!".into()),
                    name: "one".into(),
                    field_map: None,
                    module: "a".into(),
                    arity: 5,
                    location: SrcSpan {
                        start: 535,
                        end: 1100,
                    },
                    external_erlang: None,
                    external_javascript: None,
                    implementations: Implementations {
                        gleam: true,
                        uses_erlang_externals: false,
                        uses_javascript_externals: false,
                        can_run_on_erlang: true,
                        can_run_on_javascript: true,
                    },
                    purity: Purity::Pure,
                },
            },
        )]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn deprecated_module_fn_value() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                publicity: Publicity::Public,
                deprecation: Deprecation::Deprecated {
                    message: "wibble wobble".into(),
                },
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    documentation: Some("wobble!".into()),
                    name: "one".into(),
                    field_map: None,
                    module: "a".into(),
                    arity: 5,
                    location: SrcSpan {
                        start: 535,
                        end: 1100,
                    },
                    external_erlang: None,
                    external_javascript: None,
                    implementations: Implementations {
                        gleam: true,
                        uses_erlang_externals: false,
                        uses_javascript_externals: false,
                        can_run_on_erlang: true,
                        can_run_on_javascript: true,
                    },
                    purity: Purity::Pure,
                },
            },
        )]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn private_module_fn_value() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                publicity: Publicity::Private,
                deprecation: Deprecation::NotDeprecated,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    documentation: Some("wobble!".into()),
                    name: "one".into(),
                    field_map: None,
                    module: "a".into(),
                    arity: 5,
                    location: SrcSpan {
                        start: 535,
                        end: 1100,
                    },
                    external_erlang: None,
                    external_javascript: None,
                    implementations: Implementations {
                        gleam: true,
                        uses_erlang_externals: false,
                        uses_javascript_externals: false,
                        can_run_on_erlang: true,
                        can_run_on_javascript: true,
                    },
                    purity: Purity::Pure,
                },
            },
        )]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

// https://github.com/gleam-lang/gleam/commit/c8f3bd0ddbf61c27ea35f37297058ecca7515f6c
#[test]
fn module_fn_value_regression() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b/c".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                publicity: Publicity::Public,
                deprecation: Deprecation::NotDeprecated,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    documentation: Some("wabble!".into()),
                    name: "one".into(),
                    field_map: None,
                    module: "a".into(),
                    arity: 5,
                    location: SrcSpan {
                        start: 52,
                        end: 1100,
                    },
                    external_erlang: None,
                    external_javascript: None,
                    implementations: Implementations {
                        gleam: true,
                        uses_erlang_externals: false,
                        uses_javascript_externals: false,
                        can_run_on_erlang: true,
                        can_run_on_javascript: true,
                    },
                    purity: Purity::TrustedPure,
                },
            },
        )]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_fn_value_with_field_map() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                publicity: Publicity::Public,
                deprecation: Deprecation::NotDeprecated,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    documentation: Some("wubble!".into()),
                    name: "one".into(),
                    field_map: Some(FieldMap {
                        arity: 20,
                        fields: [("ok".into(), 5), ("ko".into(), 7)].into(),
                    }),
                    external_erlang: None,
                    external_javascript: None,
                    module: "a".into(),
                    arity: 5,
                    location: SrcSpan { start: 2, end: 11 },
                    implementations: Implementations {
                        gleam: true,
                        uses_erlang_externals: false,
                        uses_javascript_externals: false,
                        can_run_on_erlang: true,
                        can_run_on_javascript: true,
                    },
                    purity: Purity::Pure,
                },
            },
        )]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn record_value() {
    let mut random = rand::rng();

    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                publicity: Publicity::Public,
                deprecation: Deprecation::NotDeprecated,
                type_: type_::int(),
                variant: ValueConstructorVariant::Record {
                    documentation: Some("webble!".into()),
                    name: "one".into(),
                    module: "themodule".into(),
                    field_map: None,
                    arity: random.random(),
                    variants_count: random.random(),
                    location: SrcSpan {
                        start: random.random(),
                        end: random.random(),
                    },
                    variant_index: random.random(),
                },
            },
        )]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn record_value_with_field_map() {
    let mut random = rand::rng();

    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                publicity: Publicity::Public,
                deprecation: Deprecation::NotDeprecated,
                type_: type_::int(),
                variant: ValueConstructorVariant::Record {
                    documentation: Some("wybble!".into()),
                    module: "themodule".into(),
                    name: "one".into(),
                    field_map: Some(FieldMap {
                        arity: random.random(),
                        fields: [
                            ("ok".into(), random.random()),
                            ("ko".into(), random.random()),
                        ]
                        .into(),
                    }),
                    arity: random.random(),
                    variants_count: random.random(),
                    variant_index: random.random(),
                    location: SrcSpan {
                        start: random.random(),
                        end: random.random(),
                    },
                },
            },
        )]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn accessors() {
    let accessors1 = [
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
    ];

    let accessors2 = [(
        "a".into(),
        RecordAccessor {
            index: 1,
            label: "ok".into(),
            type_: type_::float(),
        },
    )];

    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: [
            (
                "one".into(),
                AccessorsMap {
                    publicity: Publicity::Public,
                    type_: type_::int(),
                    shared_accessors: accessors1.clone().into(),
                    variant_specific_accessors: vec![accessors1.into()],
                },
            ),
            (
                "two".into(),
                AccessorsMap {
                    publicity: Publicity::Public,
                    type_: type_::int(),
                    shared_accessors: accessors2.clone().into(),
                    variant_specific_accessors: vec![accessors2.into()],
                },
            ),
        ]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn private_accessors() {
    let accessors1 = [
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
    ];

    let accessors2 = [(
        "a".into(),
        RecordAccessor {
            index: 1,
            label: "ok".into(),
            type_: type_::float(),
        },
    )];

    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        contains_echo: false,
        values: HashMap::new(),
        accessors: [
            (
                "one".into(),
                AccessorsMap {
                    publicity: Publicity::Private,
                    type_: type_::int(),
                    shared_accessors: accessors1.clone().into(),
                    variant_specific_accessors: vec![accessors1.into()],
                },
            ),
            (
                "two".into(),
                AccessorsMap {
                    publicity: Publicity::Public,
                    type_: type_::int(),
                    shared_accessors: accessors2.clone().into(),
                    variant_specific_accessors: vec![accessors2.into()],
                },
            ),
        ]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_int() {
    let module = constant_module(Constant::Int {
        location: Default::default(),
        value: "100".into(),
        int_value: 100.into(),
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
                int_value: 1.into(),
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
                        int_value: 1.into(),
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
        type_: type_::int(),
        elements: vec![
            Constant::Int {
                location: Default::default(),
                value: "1".into(),
                int_value: 1.into(),
            },
            Constant::Int {
                location: Default::default(),
                value: "2".into(),
                int_value: 2.into(),
            },
            Constant::Int {
                location: Default::default(),
                value: "3".into(),
                int_value: 3.into(),
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
        arguments: vec![
            CallArg {
                implicit: None,
                label: None,
                location: Default::default(),
                value: Constant::Float {
                    location: Default::default(),
                    value: "0.0".into(),
                },
            },
            CallArg {
                implicit: None,
                label: None,
                location: Default::default(),
                value: Constant::Int {
                    location: Default::default(),
                    value: "1".into(),
                    int_value: 1.into(),
                },
            },
        ],
        tag: "thetag".into(),
        type_: type_::int(),
        field_map: None,
        record_constructor: None,
    });

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_var() {
    let one_original = Constant::Int {
        location: Default::default(),
        value: "1".into(),
        int_value: 1.into(),
    };

    let one = Constant::Var {
        location: Default::default(),
        module: None,
        name: "one_original".into(),
        type_: type_::int(),
        constructor: Some(Box::from(ValueConstructor {
            publicity: Publicity::Public,
            deprecation: Deprecation::NotDeprecated,
            type_: type_::int(),
            variant: ValueConstructorVariant::ModuleConstant {
                documentation: Some("some doc".into()),
                literal: one_original.clone(),
                location: SrcSpan::default(),
                module: "one/two".into(),
                implementations: Implementations {
                    gleam: true,
                    uses_erlang_externals: false,
                    uses_javascript_externals: false,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                },
                name: "one_original".into(),
            },
        })),
    };

    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [
            (
                "one".into(),
                ValueConstructor {
                    publicity: Publicity::Public,
                    deprecation: Deprecation::NotDeprecated,
                    type_: type_::int(),
                    variant: ValueConstructorVariant::ModuleConstant {
                        documentation: Some("some doc!!!!!!!!!".into()),
                        literal: one,
                        location: SrcSpan::default(),
                        module: "one/two".into(),
                        implementations: Implementations {
                            gleam: true,
                            uses_erlang_externals: false,
                            uses_javascript_externals: false,
                            can_run_on_erlang: true,
                            can_run_on_javascript: true,
                        },
                        name: "one".into(),
                    },
                },
            ),
            (
                "one_original".into(),
                ValueConstructor {
                    publicity: Publicity::Public,
                    deprecation: Deprecation::NotDeprecated,
                    type_: type_::int(),
                    variant: ValueConstructorVariant::ModuleConstant {
                        documentation: Some("some doc yeah".into()),
                        literal: one_original,
                        location: SrcSpan::default(),
                        module: "one/two".into(),
                        implementations: Implementations {
                            gleam: true,
                            uses_erlang_externals: false,
                            uses_javascript_externals: false,
                            can_run_on_erlang: true,
                            can_run_on_javascript: true,
                        },
                        name: "one_original".into(),
                    },
                },
            ),
        ]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array() {
    let module = constant_module(Constant::BitArray {
        location: Default::default(),
        segments: vec![],
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_unit() {
    let module = bit_array_segment_option_module(BitArrayOption::Unit {
        location: Default::default(),
        value: 234,
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_float() {
    let module = bit_array_segment_option_module(BitArrayOption::Float {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_int() {
    let module = bit_array_segment_option_module(BitArrayOption::Int {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_size() {
    let module = bit_array_segment_option_module(BitArrayOption::Size {
        location: Default::default(),
        value: Box::new(Constant::Int {
            location: Default::default(),
            value: "1".into(),
            int_value: 1.into(),
        }),
        short_form: false,
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_size_short_form() {
    let module = bit_array_segment_option_module(BitArrayOption::Size {
        location: Default::default(),
        value: Box::new(Constant::Int {
            location: Default::default(),
            value: "1".into(),
            int_value: 1.into(),
        }),
        short_form: true,
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_bit_arry() {
    let module = bit_array_segment_option_module(BitArrayOption::Bits {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_utf8() {
    let module = bit_array_segment_option_module(BitArrayOption::Utf8 {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_utf16() {
    let module = bit_array_segment_option_module(BitArrayOption::Utf16 {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_utf32() {
    let module = bit_array_segment_option_module(BitArrayOption::Utf32 {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_utf8codepoint() {
    let module = bit_array_segment_option_module(BitArrayOption::Utf8Codepoint {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_utf16codepoint() {
    let module = bit_array_segment_option_module(BitArrayOption::Utf16Codepoint {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_utf32codepoint() {
    let module = bit_array_segment_option_module(BitArrayOption::Utf32Codepoint {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_signed() {
    let module = bit_array_segment_option_module(BitArrayOption::Signed {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_unsigned() {
    let module = bit_array_segment_option_module(BitArrayOption::Unsigned {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_big() {
    let module = bit_array_segment_option_module(BitArrayOption::Big {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_little() {
    let module = bit_array_segment_option_module(BitArrayOption::Little {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn constant_bit_array_native() {
    let module = bit_array_segment_option_module(BitArrayOption::Native {
        location: Default::default(),
    });
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn deprecated_type() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b".into(),
        types: [(
            "ListIntType".into(),
            TypeConstructor {
                type_: type_::list(type_::int()),
                publicity: Publicity::Public,
                origin: Default::default(),
                module: "the/module".into(),
                parameters: vec![],
                deprecation: Deprecation::Deprecated {
                    message: "oh no".into(),
                },
                documentation: None,
            },
        )]
        .into(),
        types_value_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_fn_value_with_external_implementations() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b/c".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                publicity: Publicity::Public,
                deprecation: Deprecation::NotDeprecated,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    documentation: Some("wabble!".into()),
                    name: "one".into(),
                    field_map: None,
                    module: "a".into(),
                    arity: 5,
                    location: SrcSpan {
                        start: 52,
                        end: 1100,
                    },
                    external_erlang: Some(("wibble".into(), "wobble".into())),
                    external_javascript: Some(("wobble".into(), "wibble".into())),
                    implementations: Implementations {
                        gleam: false,
                        uses_erlang_externals: true,
                        uses_javascript_externals: true,
                        can_run_on_erlang: false,
                        can_run_on_javascript: true,
                    },
                    purity: Purity::Impure,
                },
            },
        )]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,
        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_containing_echo() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b/c".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [].into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: true,
        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn internal_module_fn() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b/c".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                publicity: Publicity::Internal {
                    attribute_location: None,
                },
                deprecation: Deprecation::NotDeprecated,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    documentation: Some("wabble!".into()),
                    name: "one".into(),
                    field_map: None,
                    module: "a".into(),
                    arity: 5,
                    location: SrcSpan {
                        start: 52,
                        end: 1100,
                    },
                    external_erlang: Some(("wibble".into(), "wobble".into())),
                    external_javascript: Some(("wobble".into(), "wibble".into())),
                    implementations: Implementations {
                        gleam: false,
                        uses_erlang_externals: true,
                        uses_javascript_externals: true,
                        can_run_on_erlang: true,
                        can_run_on_javascript: true,
                    },
                    purity: Purity::Unknown,
                },
            },
        )]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn internal_annotated_module_fn() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b/c".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: [(
            "one".into(),
            ValueConstructor {
                publicity: Publicity::Internal {
                    attribute_location: Some(SrcSpan {
                        start: 11,
                        end: 111,
                    }),
                },
                deprecation: Deprecation::NotDeprecated,
                type_: type_::int(),
                variant: ValueConstructorVariant::ModuleFn {
                    documentation: Some("wabble!".into()),
                    name: "one".into(),
                    field_map: None,
                    module: "a".into(),
                    arity: 5,
                    location: SrcSpan {
                        start: 52,
                        end: 1100,
                    },
                    external_erlang: Some(("wibble".into(), "wobble".into())),
                    external_javascript: Some(("wobble".into(), "wibble".into())),
                    implementations: Implementations {
                        gleam: false,
                        uses_erlang_externals: true,
                        uses_javascript_externals: true,
                        can_run_on_erlang: true,
                        can_run_on_javascript: true,
                    },
                    purity: Purity::Impure,
                },
            },
        )]
        .into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

// https://github.com/gleam-lang/gleam/issues/2599
#[test]
fn type_variable_ids_in_constructors_are_shared() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a/b/c".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::from([(
            "SomeType".into(),
            TypeVariantConstructors {
                type_parameters_ids: vec![4, 5, 6],
                variants: vec![TypeValueConstructor {
                    name: "One".into(),
                    parameters: vec![
                        TypeValueConstructorField {
                            type_: type_::generic_var(6),
                            label: None,
                        },
                        TypeValueConstructorField {
                            type_: type_::int(),
                            label: None,
                        },
                        TypeValueConstructorField {
                            type_: type_::tuple(vec![type_::generic_var(4), type_::generic_var(5)]),
                            label: None,
                        },
                    ],
                    documentation: None,
                }],
                opaque: Opaque::NotOpaque,
            },
        )]),
        accessors: HashMap::new(),
        values: [].into(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };

    let expected = HashMap::from([(
        "SomeType".into(),
        TypeVariantConstructors {
            type_parameters_ids: vec![1, 2, 0],
            variants: vec![TypeValueConstructor {
                name: "One".into(),
                parameters: vec![
                    TypeValueConstructorField {
                        type_: type_::generic_var(0),
                        label: None,
                    },
                    TypeValueConstructorField {
                        type_: type_::int(),
                        label: None,
                    },
                    TypeValueConstructorField {
                        type_: type_::tuple(vec![type_::generic_var(1), type_::generic_var(2)]),
                        label: None,
                    },
                ],
                documentation: None,
            }],
            opaque: Opaque::NotOpaque,
        },
    )]);

    assert_eq!(roundtrip(&module).types_value_constructors, expected);
}

#[test]
fn type_with_inferred_variant() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        contains_echo: false,
        origin: Origin::Src,
        name: "a/b".into(),
        types: [(
            "Wibble".into(),
            TypeConstructor {
                type_: Arc::new(Type::Named {
                    publicity: Publicity::Public,
                    package: "some_package".into(),
                    module: "the/module".into(),
                    name: "Wibble".into(),
                    arguments: Vec::new(),
                    inferred_variant: Some(1),
                }),
                publicity: Publicity::Public,
                origin: Default::default(),
                module: "the/module".into(),
                parameters: vec![],
                deprecation: Deprecation::NotDeprecated,
                documentation: None,
            },
        )]
        .into(),
        types_value_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        references: References::default(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_type_aliases() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "some_module".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: [(
            "MyAlias".into(),
            TypeAliasConstructor {
                publicity: Publicity::Public,
                module: "some_module".into(),
                type_: prelude::int(),
                arity: 1,
                deprecation: Deprecation::NotDeprecated,
                documentation: Some("Some documentation".into()),
                origin: Default::default(),
            },
        )]
        .into(),
        documentation: Vec::new(),
        contains_echo: false,

        references: References::default(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_documentation() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "some_module".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: vec![
            "This is a line of documentation".into(),
            "And here is another".into(),
            "And finally, a third".into(),
        ],
        contains_echo: false,

        references: References::default(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_opaque_type() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: [(
            "SomeType".into(),
            TypeVariantConstructors {
                type_parameters_ids: vec![0, 1, 2],
                variants: vec![TypeValueConstructor {
                    name: "One".into(),
                    parameters: vec![],
                    documentation: Some("Some documentation".into()),
                }],
                opaque: Opaque::Opaque,
            },
        )]
        .into(),
        accessors: HashMap::new(),
        values: HashMap::new(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,
        references: References::default(),
    };

    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_references() {
    let module = ModuleInterface {
        warnings: vec![],
        is_internal: false,
        package: "some_package".into(),
        origin: Origin::Src,
        name: "a".into(),
        types: HashMap::new(),
        types_value_constructors: HashMap::new(),
        accessors: HashMap::new(),
        values: HashMap::new(),
        line_numbers: LineNumbers::new(""),
        src_path: "some_path".into(),
        minimum_required_version: Version::new(0, 1, 0),
        type_aliases: HashMap::new(),
        documentation: Vec::new(),
        contains_echo: false,
        references: References {
            imported_modules: ["some_module".into(), "some_other_module".into()].into(),
            value_references: [
                (
                    ("some_module".into(), "some_function".into()),
                    vec![
                        Reference {
                            location: SrcSpan::new(1, 6),
                            kind: ReferenceKind::Definition,
                        },
                        Reference {
                            location: SrcSpan::new(7, 11),
                            kind: ReferenceKind::Unqualified,
                        },
                    ],
                ),
                (
                    ("some_other_module".into(), "some_constant".into()),
                    vec![
                        Reference {
                            location: SrcSpan::new(6, 9),
                            kind: ReferenceKind::Import,
                        },
                        Reference {
                            location: SrcSpan::new(90, 108),
                            kind: ReferenceKind::Unqualified,
                        },
                    ],
                ),
                (
                    ("some_other_module".into(), "SomeTypeVariant".into()),
                    vec![
                        Reference {
                            location: SrcSpan::new(26, 35),
                            kind: ReferenceKind::Qualified,
                        },
                        Reference {
                            location: SrcSpan::new(152, 204),
                            kind: ReferenceKind::Qualified,
                        },
                        Reference {
                            location: SrcSpan::new(0, 8),
                            kind: ReferenceKind::Qualified,
                        },
                    ],
                ),
            ]
            .into(),
            type_references: [(
                ("some_other_module".into(), "TypeVariant".into()),
                vec![
                    Reference {
                        location: SrcSpan::new(26, 35),
                        kind: ReferenceKind::Qualified,
                    },
                    Reference {
                        location: SrcSpan::new(152, 204),
                        kind: ReferenceKind::Unqualified,
                    },
                    Reference {
                        location: SrcSpan::new(0, 8),
                        kind: ReferenceKind::Definition,
                    },
                ],
            )]
            .into(),
        },
    };

    assert_eq!(roundtrip(&module), module);
}
