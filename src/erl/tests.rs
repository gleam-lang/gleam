use super::*;
use crate::typ;
use std::default::Default;

#[test]
fn record_definition_test() {
    assert_eq!(
        record_definition("PetCat", &[&"name", &"is_cute",]),
        "-record(pet_cat, {name, is_cute}).\n".to_string()
    );
}

#[test]
fn module_test() {
    use std::collections::HashMap;
    let m = Module {
        type_info: crate::typ::Module {
            name: vec!["magic".to_string()],
            types: HashMap::new(),
            values: HashMap::new(),
            accessors: HashMap::new(),
        },
        name: vec!["magic".to_string()],
        statements: vec![
            Statement::ExternalType {
                doc: None,
                location: Default::default(),
                public: true,
                name: "Any".to_string(),
                args: vec![],
            },
            Statement::CustomType {
                doc: None,
                location: Default::default(),
                public: true,
                name: "Any".to_string(),
                args: vec![],
                constructors: vec![RecordConstructor {
                    location: Default::default(),
                    name: "Ok".to_string(),
                    args: vec![],
                }],
            },
            Statement::Import {
                location: Default::default(),
                module: vec!["result".to_string()],
                as_name: None,
                unqualified: vec![],
            },
            Statement::ExternalFn {
                return_type: typ::int(),
                doc: None,
                location: Default::default(),
                args: vec![
                    ExternalFnArg {
                        location: Default::default(),
                        label: None,
                        typ: TypeAst::Constructor {
                            location: Default::default(),
                            module: None,
                            args: vec![],
                            name: "Int".to_string(),
                        },
                    },
                    ExternalFnArg {
                        location: Default::default(),
                        label: None,
                        typ: TypeAst::Constructor {
                            location: Default::default(),
                            module: None,
                            args: vec![],
                            name: "Int".to_string(),
                        },
                    },
                ],
                name: "add_ints".to_string(),
                fun: "add".to_string(),
                module: "int".to_string(),
                public: false,
                retrn: TypeAst::Constructor {
                    location: Default::default(),
                    module: None,
                    args: vec![],
                    name: "Int".to_string(),
                },
            },
            Statement::ExternalFn {
                doc: None,
                location: Default::default(),
                args: vec![],
                name: "map".to_string(),
                fun: "new".to_string(),
                module: "maps".to_string(),
                public: true,
                return_type: typ::int(),
                retrn: TypeAst::Constructor {
                    location: Default::default(),
                    module: None,
                    args: vec![],
                    name: "Map".to_string(),
                },
            },
        ],
    };
    let expected = "-module(magic).
-compile(no_auto_import).

-export([map/0]).

add_ints(A, B) ->
    int:add(A, B).

map() ->
    maps:new().
"
    .to_string();
    assert_eq!(expected, module(&m));

    let m = Module {
        type_info: crate::typ::Module {
            name: vec!["term".to_string()],
            types: HashMap::new(),
            values: HashMap::new(),
            accessors: HashMap::new(),
        },
        name: vec!["term".to_string()],
        statements: vec![
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "int".to_string(),
                body: TypedExpr::Int {
                    typ: crate::typ::int(),
                    location: Default::default(),
                    value: "176".to_string(),
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "float".to_string(),
                body: TypedExpr::Float {
                    location: Default::default(),
                    typ: crate::typ::float(),
                    value: "11177.324401".to_string(),
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "nil".to_string(),
                body: TypedExpr::ListNil {
                    location: Default::default(),
                    typ: crate::typ::int(),
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "string".to_string(),
                body: TypedExpr::String {
                    location: Default::default(),
                    typ: crate::typ::string(),
                    value: "Hello there!".to_string(),
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "seq".to_string(),
                body: TypedExpr::Seq {
                    typ: crate::typ::int(),
                    first: Box::new(TypedExpr::Int {
                        typ: crate::typ::int(),
                        location: Default::default(),
                        value: "1".to_string(),
                    }),
                    then: Box::new(TypedExpr::Int {
                        typ: crate::typ::int(),
                        location: Default::default(),
                        value: "2".to_string(),
                    }),
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "bin_op".to_string(),
                body: TypedExpr::BinOp {
                    location: Default::default(),
                    typ: crate::typ::int(),
                    name: BinOp::AddInt,
                    left: Box::new(TypedExpr::Int {
                        typ: crate::typ::int(),
                        location: Default::default(),
                        value: "1".to_string(),
                    }),
                    right: Box::new(TypedExpr::Int {
                        typ: crate::typ::int(),
                        location: Default::default(),
                        value: "2".to_string(),
                    }),
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "enum1".to_string(),
                body: TypedExpr::Var {
                    location: Default::default(),
                    constructor: ValueConstructor {
                        public: true,
                        origin: Default::default(),
                        typ: crate::typ::int(),
                        variant: ValueConstructorVariant::Record {
                            name: "Nil".to_string(),
                            field_map: None,
                            arity: 0,
                        },
                    },
                    name: "Nil".to_string(),
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "let".to_string(),
                body: TypedExpr::Let {
                    location: Default::default(),
                    typ: crate::typ::int(),
                    value: Box::new(TypedExpr::Int {
                        typ: crate::typ::int(),
                        location: Default::default(),
                        value: "1".to_string(),
                    }),
                    pattern: Pattern::Var {
                        location: Default::default(),
                        name: "OneTwo".to_string(),
                    },
                    then: Box::new(TypedExpr::Var {
                        location: Default::default(),
                        constructor: ValueConstructor {
                            public: true,
                            origin: Default::default(),
                            typ: crate::typ::int(),
                            variant: ValueConstructorVariant::LocalVariable,
                        },
                        name: "one_two".to_string(),
                    }),
                    assert: false,
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "conny".to_string(),
                body: TypedExpr::ListCons {
                    location: Default::default(),
                    typ: crate::typ::int(),
                    head: Box::new(TypedExpr::Int {
                        typ: crate::typ::int(),
                        location: Default::default(),
                        value: "12".to_string(),
                    }),
                    tail: Box::new(TypedExpr::ListCons {
                        location: Default::default(),
                        typ: crate::typ::int(),
                        head: Box::new(TypedExpr::Int {
                            typ: crate::typ::int(),
                            location: Default::default(),
                            value: "34".to_string(),
                        }),
                        tail: Box::new(TypedExpr::ListNil {
                            location: Default::default(),
                            typ: crate::typ::int(),
                        }),
                    }),
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "funny".to_string(),
                body: TypedExpr::Fn {
                    location: Default::default(),
                    is_capture: false,
                    return_annotation: None,
                    typ: crate::typ::int(),
                    args: vec![
                        Arg {
                            typ: typ::int(),
                            location: SrcSpan { start: 0, end: 0 },
                            annotation: None,
                            names: ArgNames::Named {
                                name: "one_really_long_arg_to_cause_wrapping".to_string(),
                            },
                        },
                        Arg {
                            typ: typ::int(),
                            location: SrcSpan { start: 0, end: 0 },
                            annotation: None,
                            names: ArgNames::Named {
                                name: "also_really_quite_long".to_string(),
                            },
                        },
                    ],
                    body: Box::new(TypedExpr::Int {
                        typ: crate::typ::int(),
                        location: Default::default(),
                        value: "100000000000".to_string(),
                    }),
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "tup".to_string(),
                body: TypedExpr::Tuple {
                    location: Default::default(),
                    typ: crate::typ::int(),
                    elems: vec![
                        TypedExpr::Int {
                            typ: crate::typ::int(),
                            location: Default::default(),
                            value: "1".to_string(),
                        },
                        TypedExpr::Float {
                            location: Default::default(),
                            typ: crate::typ::float(),
                            value: "2.0".to_string(),
                        },
                    ],
                },
            },
        ],
    };
    let expected = "-module(term).
-compile(no_auto_import).

int() ->
    176.

float() ->
    11177.324401.

nil() ->
    [].

string() ->
    <<\"Hello there!\"/utf8>>.

seq() ->
    1,
    2.

bin_op() ->
    1 + 2.

enum1() ->
    nil.

let() ->
    OneTwo = 1,
    OneTwo.

conny() ->
    [12, 34].

funny() ->
    fun(OneReallyLongArgToCauseWrapping, AlsoReallyQuiteLong) ->
        100000000000
    end.

tup() ->
    {1, 2.0}.
"
    .to_string();
    assert_eq!(expected, module(&m));

    let m = Module {
        type_info: crate::typ::Module {
            name: vec!["term".to_string()],
            types: HashMap::new(),
            values: HashMap::new(),
            accessors: HashMap::new(),
        },
        name: vec!["term".to_string()],
        statements: vec![Statement::Fn {
            doc: None,
            return_type: typ::int(),
            return_annotation: None,
            location: Default::default(),
            public: false,
            name: "some_function".to_string(),
            args: vec![
                Arg {
                    typ: typ::int(),
                    location: SrcSpan { start: 0, end: 0 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "arg_one".to_string(),
                    },
                },
                Arg {
                    typ: typ::int(),
                    location: SrcSpan { start: 0, end: 0 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "arg_two".to_string(),
                    },
                },
                Arg {
                    typ: typ::int(),
                    location: SrcSpan { start: 0, end: 0 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "arg_3".to_string(),
                    },
                },
                Arg {
                    typ: typ::int(),
                    location: SrcSpan { start: 0, end: 0 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "arg4".to_string(),
                    },
                },
                Arg {
                    typ: typ::int(),
                    location: SrcSpan { start: 0, end: 0 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "arg_four".to_string(),
                    },
                },
                Arg {
                    typ: typ::int(),
                    location: SrcSpan { start: 0, end: 0 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "arg__five".to_string(),
                    },
                },
                Arg {
                    typ: typ::int(),
                    location: SrcSpan { start: 0, end: 0 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "arg_six".to_string(),
                    },
                },
                Arg {
                    typ: typ::int(),
                    location: SrcSpan { start: 0, end: 0 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "arg_that_is_long".to_string(),
                    },
                },
            ],
            body: TypedExpr::Int {
                typ: crate::typ::int(),
                location: Default::default(),
                value: "1".to_string(),
            },
        }],
    };
    let expected = "-module(term).
-compile(no_auto_import).

some_function(
    ArgOne,
    ArgTwo,
    Arg3,
    Arg4,
    ArgFour,
    ArgFive,
    ArgSix,
    ArgThatIsLong
) ->
    1.
"
    .to_string();
    assert_eq!(expected, module(&m));

    let m = Module {
        type_info: crate::typ::Module {
            name: vec!["ok".to_string()],
            types: HashMap::new(),
            values: HashMap::new(),
            accessors: HashMap::new(),
        },
        name: vec!["vars".to_string()],
        statements: vec![
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "arg".to_string(),
                body: TypedExpr::Var {
                    location: Default::default(),
                    name: "some_arg".to_string(),
                    constructor: ValueConstructor {
                        public: true,
                        origin: Default::default(),
                        typ: crate::typ::int(),
                        variant: ValueConstructorVariant::LocalVariable,
                    },
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "moddy".to_string(),
                body: TypedExpr::ModuleSelect {
                    typ: crate::typ::fn_(vec![], crate::typ::int()),
                    location: Default::default(),
                    module_alias: "zero".to_string(),
                    module_name: vec!["one".to_string()],
                    label: "two".to_string(),
                    constructor: ModuleValueConstructor::Fn,
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "moddy2".to_string(),
                body: TypedExpr::ModuleSelect {
                    typ: crate::typ::fn_(
                        vec![crate::typ::int(), crate::typ::int()],
                        crate::typ::int(),
                    ),
                    location: Default::default(),
                    module_alias: "zero".to_string(),
                    module_name: vec!["one".to_string(), "zero".to_string()],
                    label: "two".to_string(),
                    constructor: ModuleValueConstructor::Fn,
                },
            },
            Statement::Fn {
                doc: None,
                return_annotation: None,
                return_type: typ::int(),
                location: Default::default(),
                public: false,
                args: vec![],
                name: "moddy4".to_string(),
                body: TypedExpr::Call {
                    location: Default::default(),
                    typ: crate::typ::int(),
                    args: vec![CallArg {
                        label: None,
                        location: Default::default(),
                        value: TypedExpr::Int {
                            location: Default::default(),
                            typ: crate::typ::int(),
                            value: "1".to_string(),
                        },
                    }],
                    fun: Box::new(TypedExpr::ModuleSelect {
                        typ: crate::typ::int(),
                        location: Default::default(),
                        module_alias: "zero".to_string(),
                        module_name: vec!["one".to_string(), "zero".to_string()],
                        label: "two".to_string(),
                        constructor: ModuleValueConstructor::Fn,
                    }),
                },
            },
        ],
    };
    let expected = "-module(vars).
-compile(no_auto_import).

arg() ->
    SomeArg.

moddy() ->
    fun one:two/0.

moddy2() ->
    fun one@zero:two/2.

moddy4() ->
    one@zero:two(1).
"
    .to_string();
    assert_eq!(expected, module(&m));

    let m = Module {
        type_info: crate::typ::Module {
            name: vec!["my_mod".to_string()],
            types: HashMap::new(),
            values: HashMap::new(),
            accessors: HashMap::new(),
        },
        name: vec!["my_mod".to_string()],
        statements: vec![Statement::Fn {
            doc: None,
            return_type: typ::int(),
            return_annotation: None,
            location: Default::default(),
            public: false,
            args: vec![],
            name: "go".to_string(),
            body: TypedExpr::Case {
                location: Default::default(),
                typ: crate::typ::int(),
                subjects: vec![TypedExpr::Int {
                    typ: crate::typ::int(),
                    location: Default::default(),
                    value: "1".to_string(),
                }],
                clauses: vec![
                    Clause {
                        location: Default::default(),
                        guard: None,
                        pattern: vec![Pattern::Int {
                            location: Default::default(),
                            value: "1".to_string(),
                        }],
                        alternative_patterns: vec![],
                        then: TypedExpr::Int {
                            typ: crate::typ::int(),
                            location: Default::default(),
                            value: "1".to_string(),
                        },
                    },
                    Clause {
                        location: Default::default(),
                        guard: None,
                        pattern: vec![Pattern::Float {
                            location: Default::default(),
                            value: "1.0".to_string(),
                        }],
                        alternative_patterns: vec![],
                        then: TypedExpr::Int {
                            typ: crate::typ::int(),
                            location: Default::default(),
                            value: "1".to_string(),
                        },
                    },
                    Clause {
                        location: Default::default(),
                        guard: None,
                        pattern: vec![Pattern::String {
                            location: Default::default(),
                            value: "hello".to_string(),
                        }],
                        alternative_patterns: vec![],
                        then: TypedExpr::Int {
                            typ: crate::typ::int(),
                            location: Default::default(),
                            value: "1".to_string(),
                        },
                    },
                    Clause {
                        location: Default::default(),
                        guard: None,
                        pattern: vec![Pattern::Nil {
                            location: Default::default(),
                        }],
                        alternative_patterns: vec![],
                        then: TypedExpr::Int {
                            typ: crate::typ::int(),
                            location: Default::default(),
                            value: "1".to_string(),
                        },
                    },
                    Clause {
                        location: Default::default(),
                        guard: None,
                        pattern: vec![Pattern::Constructor {
                            location: Default::default(),
                            module: None,
                            name: "Error".to_string(),
                            args: vec![CallArg {
                                label: None,
                                location: Default::default(),
                                value: Pattern::Int {
                                    location: Default::default(),
                                    value: "2".to_string(),
                                },
                            }],
                            constructor: PatternConstructor::Record {
                                name: "Error".to_string(),
                            },
                        }],
                        alternative_patterns: vec![],
                        then: TypedExpr::Int {
                            typ: crate::typ::int(),
                            location: Default::default(),
                            value: "1".to_string(),
                        },
                    },
                    Clause {
                        location: Default::default(),
                        guard: None,
                        pattern: vec![Pattern::Tuple {
                            location: Default::default(),
                            elems: vec![
                                Pattern::Int {
                                    location: Default::default(),
                                    value: "1".to_string(),
                                },
                                Pattern::Int {
                                    location: Default::default(),
                                    value: "2".to_string(),
                                },
                            ],
                        }],
                        alternative_patterns: vec![],
                        then: TypedExpr::Int {
                            typ: crate::typ::int(),
                            location: Default::default(),
                            value: "1".to_string(),
                        },
                    },
                ],
            },
        }],
    };
    let expected = "-module(my_mod).
-compile(no_auto_import).

go() ->
    case 1 of
        1 ->
            1;

        1.0 ->
            1;

        <<\"hello\"/utf8>> ->
            1;

        [] ->
            1;

        {error, 2} ->
            1;

        {1, 2} ->
            1
    end.
"
    .to_string();
    assert_eq!(expected, module(&m));

    let m = Module {
        type_info: crate::typ::Module {
            name: vec!["funny".to_string()],
            types: HashMap::new(),
            values: HashMap::new(),
            accessors: HashMap::new(),
        },
        name: vec!["funny".to_string()],
        statements: vec![
            Statement::Fn {
                doc: None,
                return_type: typ::int(),
                return_annotation: None,
                location: Default::default(),
                args: vec![],
                name: "one".to_string(),
                public: false,
                body: TypedExpr::Call {
                    location: Default::default(),
                    typ: crate::typ::int(),
                    args: vec![CallArg {
                        label: None,
                        location: Default::default(),
                        value: TypedExpr::Int {
                            typ: crate::typ::int(),
                            location: Default::default(),
                            value: "1".to_string(),
                        },
                    }],
                    fun: Box::new(TypedExpr::Var {
                        location: Default::default(),
                        constructor: ValueConstructor {
                            public: true,
                            origin: Default::default(),
                            variant: ValueConstructorVariant::ModuleFn {
                                name: "one_two".to_string(),
                                module: vec!["funny".to_string()],
                                field_map: None,
                                arity: 1,
                            },
                            typ: crate::typ::int(),
                        },
                        name: "one_two".to_string(),
                    }),
                },
            },
            Statement::Fn {
                doc: None,
                return_type: typ::int(),
                return_annotation: None,
                location: Default::default(),
                args: vec![],
                name: "two".to_string(),
                public: false,
                body: TypedExpr::Call {
                    location: Default::default(),
                    typ: crate::typ::int(),
                    args: vec![CallArg {
                        label: None,
                        location: Default::default(),
                        value: TypedExpr::Int {
                            typ: crate::typ::int(),
                            location: Default::default(),
                            value: "1".to_string(),
                        },
                    }],
                    fun: Box::new(TypedExpr::Var {
                        location: Default::default(),
                        constructor: ValueConstructor {
                            public: true,
                            origin: Default::default(),
                            variant: ValueConstructorVariant::LocalVariable,
                            typ: crate::typ::int(),
                        },
                        name: "one_two".to_string(),
                    }),
                },
            },
            Statement::Fn {
                doc: None,
                return_type: typ::int(),
                return_annotation: None,
                location: Default::default(),
                args: vec![],
                name: "three".to_string(),
                public: false,
                body: TypedExpr::Call {
                    location: Default::default(),
                    typ: crate::typ::int(),
                    args: vec![CallArg {
                        label: None,
                        location: Default::default(),
                        value: TypedExpr::Int {
                            typ: crate::typ::int(),
                            location: Default::default(),
                            value: "2".to_string(),
                        },
                    }],
                    fun: Box::new(TypedExpr::Call {
                        location: Default::default(),
                        typ: crate::typ::int(),
                        args: vec![CallArg {
                            label: None,
                            location: Default::default(),
                            value: TypedExpr::Int {
                                typ: crate::typ::int(),
                                location: Default::default(),
                                value: "1".to_string(),
                            },
                        }],
                        fun: Box::new(TypedExpr::Var {
                            location: Default::default(),
                            constructor: ValueConstructor {
                                public: true,
                                origin: Default::default(),
                                typ: crate::typ::int(),
                                variant: ValueConstructorVariant::ModuleFn {
                                    name: "one_two_actual".to_string(),
                                    module: vec!["funny".to_string()],
                                    field_map: None,
                                    arity: 2,
                                },
                            },
                            name: "one_two_alias".to_string(),
                        }),
                    }),
                },
            },
        ],
    };
    let expected = "-module(funny).
-compile(no_auto_import).

one() ->
    one_two(1).

two() ->
    OneTwo(1).

three() ->
    (one_two_actual(1))(2).
"
    .to_string();
    assert_eq!(expected, module(&m));
}

#[test]
fn integration_test() {
    macro_rules! assert_erl {
        ($src:expr, $erl:expr $(,)?) => {
            let mut ast = crate::grammar::ModuleParser::new()
                .parse($src)
                .expect("syntax error");
            ast.name = vec!["the_app".to_string()];
            let ast = crate::typ::infer_module(ast, &std::collections::HashMap::new())
                .expect("should successfully infer");
            let output = module(&ast);
            assert_eq!(($src, output), ($src, $erl.to_string()));
        };
    }

    assert_erl!(
        r#"fn go() {
let x = tuple(100000000000000000, tuple(2000000000, 3000000000000, 40000000000), 50000, 6000000000)
  x
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

go() ->
    X = {100000000000000000,
         {2000000000, 3000000000000, 40000000000},
         50000,
         6000000000},
    X.
"#,
    );

    assert_erl!(
        r#"fn go() {
  let y = 1
  let y = 2
  y
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

go() ->
    Y = 1,
    Y1 = 2,
    Y1.
"#,
    );

    assert_erl!(
        r#"fn go() {
  assert y = 1
  assert y = 2
  y
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

go() ->
    Y = 1,
    Y1 = 2,
    Y1.
"#,
    );

    assert_erl!(
        r#"pub fn t() { True }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([t/0]).

t() ->
    true.
"#,
    );

    assert_erl!(
        r#"pub type Money { Pound(Int) }
                    fn pound(x) { Pound(x) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

pound(X) ->
    {pound, X}.
"#,
    );

    assert_erl!(
        r#"fn loop() { loop() }"#,
        r#"-module(the_app).
-compile(no_auto_import).

loop() ->
    loop().
"#,
    );

    assert_erl!(
        r#"external fn run() -> Int = "Elixir.MyApp" "run""#,
        r#"-module(the_app).
-compile(no_auto_import).

run() ->
    'Elixir.MyApp':run().
"#,
    );

    assert_erl!(
        r#"fn inc(x) { x + 1 }
                    pub fn go() { 1 |> inc |> inc |> inc }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([go/0]).

inc(X) ->
    X + 1.

go() ->
    inc(inc(inc(1))).
"#,
    );

    assert_erl!(
        r#"fn add(x, y) { x + y }
                    pub fn go() { 1 |> add(_, 1) |> add(2, _) |> add(_, 3) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([go/0]).

add(X, Y) ->
    X + Y.

go() ->
    add(add(2, add(1, 1)), 3).
"#,
    );

    assert_erl!(
        r#"fn and(x, y) { x && y }
                    fn or(x, y) { x || y }
                    fn modulo(x, y) { x % y }
            "#,
        r#"-module(the_app).
-compile(no_auto_import).

'and'(X, Y) ->
    X andalso Y.

'or'(X, Y) ->
    X orelse Y.

modulo(X, Y) ->
    X rem Y.
"#,
    );

    assert_erl!(
        r#"fn second(list) { case list { [x, y] -> y z -> 1 } }
                    fn tail(list) { case list { [x, ..xs] -> xs z -> list } }
            "#,
        r#"-module(the_app).
-compile(no_auto_import).

second(List) ->
    case List of
        [X, Y] ->
            Y;

        Z ->
            1
    end.

tail(List) ->
    case List of
        [X | Xs] ->
            Xs;

        Z ->
            List
    end.
"#,
    );

    // Deprecated syntax
    assert_erl!(
        r#"fn second(list) { case list { [x, y] -> y z -> 1 } }
                    fn tail(list) { case list { [x | xs] -> xs z -> list } }
            "#,
        r#"-module(the_app).
-compile(no_auto_import).

second(List) ->
    case List of
        [X, Y] ->
            Y;

        Z ->
            1
    end.

tail(List) ->
    case List of
        [X | Xs] ->
            Xs;

        Z ->
            List
    end.
"#,
    );

    assert_erl!(
        r#"fn x() { let x = 1 let x = x + 1 x }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    X = 1,
    X1 = X + 1,
    X1.
"#,
    );

    assert_erl!(
        r#"pub external fn receive() -> Int = "try" "and"
                    pub fn catch(x) { receive() }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export(['receive'/0, 'catch'/1]).

'receive'() ->
    'try':'and'().

'catch'(X) ->
    'receive'().
"#,
    );

    // Translation of Float-specific BinOp into variable-type Erlang term comparison.
    assert_erl!(
        r#"fn x() { 1. <. 2.3 }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    1.0 < 2.3.
"#,
    );

    // Custom type creation
    assert_erl!(
        r#"type Pair(x, y) { Pair(x: x, y: y) } fn x() { Pair(1, 2) Pair(3., 4.) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    {pair, 1, 2},
    {pair, 3.0, 4.0}.
"#,
    );

    assert_erl!(
        r#"type Null { Null } fn x() { Null }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    null.
"#,
    );

    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) }
                fn y() { fn() { Point }()(4, 6) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

y() ->
    ((fun() -> fun(A, B) -> {point, A, B} end end)())(4, 6).
"#,
    );

    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) }
                fn x() { Point(x: 4, y: 6) Point(y: 1, x: 9) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    {point, 4, 6},
    {point, 9, 1}.
"#,
    );

    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) } fn x(y) { let Point(a, b) = y a }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x(Y) ->
    {point, A, B} = Y,
    A.
"#,
    );

    assert_erl!(
        r#"external fn go(x: Int, y: Int) -> Int = "m" "f"
                    fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

go(A, B) ->
    m:f(A, B).

x() ->
    go(1, 2),
    go(4, 3).
"#,
    );

    assert_erl!(
        r#"fn go(x xx, y yy) { xx }
                    fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

go(Xx, Yy) ->
    Xx.

x() ->
    go(1, 2),
    go(4, 3).
"#,
    );

    // https://github.com/gleam-lang/gleam/issues/289
    assert_erl!(
        r#"
type User { User(id: Int, name: String, age: Int) }
fn create_user(user_id) { User(age: 22, id: user_id, name: "") }
                    "#,
        r#"-module(the_app).
-compile(no_auto_import).

create_user(UserId) ->
    {user, UserId, <<""/utf8>>, 22}.
"#,
    );

    assert_erl!(
        r#"fn run() { case 1, 2 { a, b -> a } }"#,
        r#"-module(the_app).
-compile(no_auto_import).

run() ->
    case {1, 2} of
        {A, B} ->
            A
    end.
"#,
    );

    assert_erl!(
        r#"type X { X(x: Int, y: Float) }
                    fn x() { X(x: 1, y: 2.) X(y: 3., x: 4) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    {x, 1, 2.0},
    {x, 4, 3.0}.
"#,
    );

    // https://github.com/gleam-lang/gleam/issues/333
    assert_erl!(
        r#"
fn go(a) {
  case a {
    99 -> {
      let a = a
      1
    }
    _ -> a
  }
}

                    "#,
        r#"-module(the_app).
-compile(no_auto_import).

go(A) ->
    case A of
        99 ->
            A1 = A,
            1;

        _ ->
            A
    end.
"#,
    );

    assert_erl!(
        r#"
fn go(a) {
  let a = a + 1
  a
}

                    "#,
        r#"-module(the_app).
-compile(no_auto_import).

go(A) ->
    A1 = A + 1,
    A1.
"#,
    );

    assert_erl!(
        r#"
fn go(a) {
  let a = 1
  a
}

                    "#,
        r#"-module(the_app).
-compile(no_auto_import).

go(A) ->
    A1 = 1,
    A1.
"#,
    );

    assert_erl!(
        r#"
fn id(x) {
  x
}

fn main() {
  id(id)
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

id(X) ->
    X.

main() ->
    id(fun id/1).
"#,
    );

    // https://github.com/gleam-lang/gleam/issues/358
    assert_erl!(
        r#"
pub fn factory(f, i) {
  f(i)
}

pub type Box {
  Box(i: Int)
}

pub fn main() {
  factory(Box, 0)
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([factory/2, main/0]).

factory(F, I) ->
    F(I).

main() ->
    factory(fun(A) -> {box, A} end, 0).
"#,
    );

    // https://github.com/gleam-lang/gleam/issues/384
    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    _ -> {
      let a = 1
      a
    }
  }
  let a = 2
  a
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(Args) ->
    case Args of
        _ ->
            A = 1,
            A
    end,
    A1 = 2,
    A1.
"#,
    );

    // Clause guards
    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    x if x == args -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(Args) ->
    case Args of
        X when X =:= Args ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    x if {x != x} == {args == args} -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(Args) ->
    case Args of
        X when (X =/= X) =:= (Args =:= Args) ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    x if x && x || x == x && x -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(Args) ->
    case Args of
        X when (X andalso X) orelse ((X =:= X) andalso X) ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x > y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {1, 0} of
        {X, Y} when X > Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x >= y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {1, 0} of
        {X, Y} when X >= Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x < y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {1, 0} of
        {X, Y} when X < Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x <= y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {1, 0} of
        {X, Y} when X =< Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1.0, 0.1 {
    x, y if x >. y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {1.0, 0.1} of
        {X, Y} when X > Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1.0, 0.1 {
    x, y if x >=. y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {1.0, 0.1} of
        {X, Y} when X >= Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0.123
  case x {
    99.9854 -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = 0.123,
    case X of
        99.9854 ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0.123
  case x {
    _ if x == 3.14 -> 1
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = 0.123,
    case X of
        _ when X =:= 3.14 ->
            1
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0.123
  case x {
    _ if 0.123 <. x -> 1
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = 0.123,
    case X of
        _ when 0.123 < X ->
            1
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0
  case x {
    0 -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = 0,
    case X of
        0 ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0
  case x {
    _ if x == 0 -> 1
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = 0,
    case X of
        _ when X =:= 0 ->
            1
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0
  case x {
    _ if 0 < x -> 1
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = 0,
    case X of
        _ when 0 < X ->
            1
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 0.1, 1.0 {
    x, y if x <. y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {0.1, 1.0} of
        {X, Y} when X < Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 0.1, 1.0 {
    x, y if x <=. y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {0.1, 1.0} of
        {X, Y} when X =< Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    [x] | [x, _] if x -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(Args) ->
    case Args of
        [X] when X ->
            1;

        [X1, _] when X1 ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  todo
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    erlang:error({gleam_error, todo}).
"#,
    );

    // We can use record accessors for types with only one constructor
    assert_erl!(
        r#"
pub type Person { Person(name: String, age: Int) }
pub fn get_age(person: Person) { person.age }
pub fn get_name(person: Person) { person.name }
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([get_age/1, get_name/1]).

get_age(Person) ->
    erlang:element(3, Person).

get_name(Person) ->
    erlang:element(2, Person).
"#,
    );

    // a |> b
    assert_erl!(
        r#"
pub fn apply(f: fn(a) -> b, a: a) { a |> f }
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([apply/2]).

apply(F, A) ->
    F(A).
"#,
    );

    // a |> b(c)
    assert_erl!(
        r#"
pub fn apply(f: fn(a, Int) -> b, a: a) { a |> f(1) }
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([apply/2]).

apply(F, A) ->
    F(A, 1).
"#,
    );
}
