use crate::ast::{
    Arg, BinOp, Clause, EnumConstructor, Expr, Meta, Module, Pattern, Statement, Type,
};
use crate::grammar::{ExprParser, ModuleParser};

pub fn meta(start: usize, end: usize) -> Meta {
    Meta { start, end }
}

#[test]
fn expr_test() {
    assert_eq!(
        Ok(Expr::Int {
            typ: (),
            meta: Meta { start: 0, end: 3 },
            value: 123
        }),
        ExprParser::new().parse("123"),
    );

    assert_eq!(
        Ok(Expr::Int {
            typ: (),
            meta: Meta { start: 0, end: 3 },
            value: -45
        }),
        ExprParser::new().parse("-45"),
    );

    assert_eq!(
        Ok(Expr::Float {
            typ: (),
            meta: Meta { start: 0, end: 3 },
            value: -1.0
        }),
        ExprParser::new().parse("-1."),
    );

    assert_eq!(
        Ok(Expr::Float {
            typ: (),
            meta: Meta { start: 0, end: 4 },
            value: 1.23
        }),
        ExprParser::new().parse("1.23"),
    );

    assert_eq!(
        Ok(Expr::Float {
            typ: (),
            meta: Meta { start: 0, end: 5 },
            value: -1.23
        }),
        ExprParser::new().parse("-1.23"),
    );

    assert_eq!(
        Ok(Expr::String {
            typ: (),
            meta: Meta { start: 0, end: 15 },
            value: "Hello, world!".to_string(),
        }),
        ExprParser::new().parse(r#""Hello, world!""#),
    );

    assert_eq!(
        Ok(Expr::String {
            typ: (),
            meta: Meta { start: 0, end: 13 },
            value: "quote -> \\\"".to_string(),
        }),
        ExprParser::new().parse(r#""quote -> \"""#),
    );

    assert_eq!(
        Ok(Expr::Atom {
            typ: (),
            meta: Meta { start: 0, end: 15 },
            value: "Hello, world!".to_string(),
        }),
        ExprParser::new().parse(r#"'Hello, world!'"#),
    );

    assert_eq!(
        Ok(Expr::String {
            typ: (),
            meta: Meta { start: 0, end: 13 },
            value: "quote -> \\'".to_string(),
        }),
        ExprParser::new().parse(r#""quote -> \'""#),
    );

    assert_eq!(
        Ok(Expr::Nil {
            meta: Meta { start: 0, end: 2 },
            typ: (),
        }),
        ExprParser::new().parse("[]"),
    );

    assert_eq!(
        Ok(Expr::RecordNil {
            meta: Meta { start: 0, end: 2 },
            typ: (),
        }),
        ExprParser::new().parse("{}"),
    );

    assert_eq!(
        Ok(Expr::Call {
            meta: Meta { start: 0, end: 11 },
            typ: (),
            fun: Box::new(Expr::Var {
                meta: Meta { start: 0, end: 5 },
                typ: (),
                name: "hello".to_string(),
                scope: (),
            }),
            args: vec![
                Expr::Int {
                    typ: (),
                    meta: Meta { start: 6, end: 7 },
                    value: 1
                },
                Expr::Int {
                    typ: (),
                    meta: Meta { start: 9, end: 10 },
                    value: 2
                },
            ]
        }),
        ExprParser::new().parse("hello(1, 2)"),
    );

    assert_eq!(
        Ok(Expr::Var {
            meta: Meta { start: 0, end: 5 },
            typ: (),
            name: "hello".to_string(),
            scope: (),
        }),
        ExprParser::new().parse("hello"),
    );

    assert_eq!(
        Ok(Expr::Call {
            meta: Meta { start: 0, end: 10 },
            typ: (),
            fun: Box::new(Expr::Constructor {
                meta: Meta { start: 0, end: 4 },
                typ: (),
                name: "Pair".to_string(),
            }),
            args: vec![
                Expr::Int {
                    typ: (),
                    meta: Meta { start: 5, end: 6 },
                    value: 1
                },
                Expr::Int {
                    typ: (),
                    meta: Meta { start: 8, end: 9 },
                    value: 3
                }
            ]
        }),
        ExprParser::new().parse("Pair(1, 3)"),
    );

    assert_eq!(
        Ok(Expr::Cons {
            meta: Meta { start: 0, end: 8 },
            typ: (),
            head: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 1, end: 2 },
                value: 1
            }),
            tail: Box::new(Expr::Nil {
                meta: Meta { start: 5, end: 7 },
                typ: (),
            })
        }),
        ExprParser::new().parse("[1 | []]"),
    );

    assert_eq!(
        Ok(Expr::Cons {
            meta: Meta { start: 1, end: 2 },
            typ: (),
            head: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 1, end: 2 },
                value: 1
            }),
            tail: Box::new(Expr::Nil {
                meta: Meta { start: 2, end: 3 },
                typ: (),
            })
        }),
        ExprParser::new().parse("[1]"),
    );

    assert_eq!(
        Ok(Expr::Let {
            meta: Meta { start: 0, end: 10 },
            typ: (),
            value: Box::new(Expr::BinOp {
                meta: Meta { start: 4, end: 9 },
                typ: (),
                name: BinOp::AddInt,
                left: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 4, end: 5 },
                    value: 1
                }),
                right: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 8, end: 9 },
                    value: 2
                }),
            }),
            pattern: Pattern::Var {
                meta: Meta { start: 0, end: 1 },
                name: "x".to_string(),
            },
            then: Box::new(Expr::Var {
                typ: (),
                scope: (),
                meta: Meta { start: 10, end: 11 },
                name: "x".to_string(),
            })
        }),
        ExprParser::new().parse("x = 1 + 2 x"),
    );

    assert_eq!(
        Ok(Expr::BinOp {
            meta: Meta { start: 0, end: 5 },
            typ: (),
            name: BinOp::AddInt,
            left: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 0, end: 1 },
                value: 1
            }),
            right: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 4, end: 5 },
                value: 2
            }),
        }),
        ExprParser::new().parse("1 + 2"),
    );

    assert_eq!(
        Ok(Expr::BinOp {
            meta: Meta { start: 0, end: 9 },
            typ: (),
            name: BinOp::AddInt,
            left: Box::new(Expr::BinOp {
                meta: Meta { start: 0, end: 5 },
                typ: (),
                name: BinOp::AddInt,
                left: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 0, end: 1 },
                    value: 1
                }),
                right: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 4, end: 5 },
                    value: 2
                }),
            }),
            right: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 8, end: 9 },
                value: 3
            }),
        }),
        ExprParser::new().parse("1 + 2 + 3"),
    );

    assert_eq!(
        Ok(Expr::BinOp {
            meta: Meta { start: 0, end: 10 },
            typ: (),
            name: BinOp::AddFloat,
            left: Box::new(Expr::BinOp {
                meta: Meta { start: 0, end: 5 },
                typ: (),
                name: BinOp::AddInt,
                left: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 0, end: 1 },
                    value: 1
                }),
                right: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 4, end: 5 },
                    value: 2
                }),
            }),
            right: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 9, end: 10 },
                value: 3
            }),
        }),
        ExprParser::new().parse("1 + 2 +. 3"),
    );

    assert_eq!(
        Ok(Expr::BinOp {
            meta: Meta { start: 0, end: 9 },
            typ: (),
            name: BinOp::AddInt,
            left: Box::new(Expr::BinOp {
                meta: Meta { start: 0, end: 5 },
                typ: (),
                name: BinOp::MultInt,
                left: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 0, end: 1 },
                    value: 1
                }),
                right: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 4, end: 5 },
                    value: 2
                }),
            }),
            right: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 8, end: 9 },
                value: 3
            }),
        }),
        ExprParser::new().parse("1 * 2 + 3"),
    );

    assert_eq!(
        Ok(Expr::BinOp {
            meta: Meta { start: 0, end: 9 },
            typ: (),
            name: BinOp::AddInt,
            left: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 0, end: 1 },
                value: 1
            }),
            right: Box::new(Expr::BinOp {
                meta: Meta { start: 4, end: 9 },
                typ: (),
                name: BinOp::MultInt,
                left: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 4, end: 5 },
                    value: 2
                }),
                right: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 8, end: 9 },
                    value: 3
                }),
            }),
        }),
        ExprParser::new().parse("1 + 2 * 3"),
    );

    assert_eq!(
        Ok(Expr::Cons {
            meta: Meta { start: 1, end: 2 },
            typ: (),
            head: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 1, end: 2 },
                value: 1
            }),
            tail: Box::new(Expr::Cons {
                meta: Meta { start: 4, end: 5 },
                typ: (),
                head: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 4, end: 5 },
                    value: 2
                }),
                tail: Box::new(Expr::Nil {
                    meta: Meta { start: 5, end: 6 },
                    typ: (),
                })
            })
        }),
        ExprParser::new().parse("[1, 2]"),
    );

    assert_eq!(
        Ok(Expr::Let {
            meta: Meta { start: 0, end: 6 },
            typ: (),
            value: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 4, end: 5 },
                value: 1
            }),
            pattern: Pattern::Var {
                meta: Meta { start: 0, end: 1 },
                name: "x".to_string(),
            },
            then: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 6, end: 7 },
                value: 2
            })
        }),
        ExprParser::new().parse("x = 1 2"),
    );

    assert_eq!(
        Ok(Expr::Seq {
            meta: Meta { start: 0, end: 3 },
            typ: (),
            first: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 0, end: 1 },
                value: 1
            }),
            then: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 2, end: 3 },
                value: 2
            }),
        }),
        ExprParser::new().parse("1 2"),
    );

    assert_eq!(
        Ok(Expr::RecordSelect {
            meta: Meta { start: 0, end: 11 },
            typ: (),
            label: "name".to_string(),
            record: Box::new(Expr::Var {
                meta: Meta { start: 0, end: 6 },
                typ: (),
                name: "person".to_string(),
                scope: (),
            })
        }),
        ExprParser::new().parse("person.name"),
    );

    assert_eq!(
        Ok(Expr::ModuleSelect {
            meta: Meta { start: 0, end: 11 },
            typ: (),
            label: "name".to_string(),
            module: Box::new(Expr::Var {
                meta: Meta { start: 0, end: 6 },
                typ: (),
                name: "person".to_string(),
                scope: (),
            })
        }),
        ExprParser::new().parse("person:name"),
    );

    assert_eq!(
        Ok(Expr::Tuple {
            meta: Meta { start: 0, end: 9 },
            typ: (),
            elems: vec![
                Expr::Int {
                    typ: (),
                    meta: Meta { start: 1, end: 2 },
                    value: 1
                },
                Expr::Int {
                    typ: (),
                    meta: Meta { start: 4, end: 5 },
                    value: 2
                },
                Expr::Int {
                    typ: (),
                    meta: Meta { start: 7, end: 8 },
                    value: 3
                },
            ]
        }),
        ExprParser::new().parse("{1, 2, 3}"),
    );

    assert_eq!(
        Ok(Expr::Fn {
            meta: Meta { start: 0, end: 10 },
            typ: (),
            args: vec![],
            body: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 7, end: 8 },
                value: 1
            })
        }),
        ExprParser::new().parse("fn() { 1 }"),
    );

    assert_eq!(
        Ok(Expr::Fn {
            meta: Meta { start: 0, end: 16 },
            typ: (),
            args: vec![
                Arg {
                    name: "a".to_string()
                },
                Arg {
                    name: "b".to_string()
                },
            ],
            body: Box::new(Expr::Seq {
                meta: Meta { start: 11, end: 14 },
                typ: (),
                first: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 11, end: 12 },
                    value: 1
                }),
                then: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 13, end: 14 },
                    value: 2
                })
            })
        }),
        ExprParser::new().parse("fn(a, b) { 1 2 }"),
    );

    assert_eq!(
        Ok(Expr::Let {
            meta: Meta { start: 0, end: 6 },
            typ: (),
            value: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 4, end: 5 },
                value: 1
            }),
            pattern: Pattern::Int {
                meta: Meta { start: 0, end: 1 },
                value: 0,
            },
            then: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 6, end: 7 },
                value: 2
            })
        }),
        ExprParser::new().parse("0 = 1 2"),
    );

    assert_eq!(
        Ok(Expr::Let {
            meta: Meta { start: 0, end: 8 },
            typ: (),
            value: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 6, end: 7 },
                value: 1,
            }),
            pattern: Pattern::Float {
                meta: Meta { start: 0, end: 3 },
                value: 1.0,
            },
            then: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 8, end: 9 },
                value: 2
            })
        }),
        ExprParser::new().parse("1.0 = 1 2"),
    );

    assert_eq!(
        Ok(Expr::Let {
            meta: Meta { start: 0, end: 8 },
            typ: (),
            value: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 6, end: 7 },
                value: 1
            }),
            pattern: Pattern::Atom {
                meta: Meta { start: 0, end: 3 },
                value: "a".to_string(),
            },
            then: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 8, end: 9 },
                value: 2
            })
        }),
        ExprParser::new().parse("'a' = 1 2"),
    );

    assert_eq!(
        Ok(Expr::Let {
            meta: Meta { start: 0, end: 8 },
            typ: (),
            value: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 6, end: 7 },
                value: 1
            }),
            pattern: Pattern::String {
                meta: Meta { start: 0, end: 3 },
                value: "a".to_string(),
            },
            then: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 8, end: 9 },
                value: 2
            })
        }),
        ExprParser::new().parse("\"a\" = 1 2"),
    );

    assert_eq!(
        Ok(Expr::RecordCons {
            meta: Meta { start: 0, end: 19 },
            typ: (),
            label: "size".to_string(),
            value: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 16, end: 17 },
                value: 2
            }),
            tail: Box::new(Expr::Var {
                meta: Meta { start: 2, end: 6 },
                typ: (),
                name: "jane".to_string(),
                scope: (),
            }),
        }),
        ExprParser::new().parse("{ jane | size = 2 }"),
    );

    assert_eq!(
        Ok(Expr::Case {
            meta: Meta { start: 0, end: 30 },
            typ: (),
            subject: Box::new(Expr::Var {
                typ: (),
                scope: (),
                meta: Meta { start: 5, end: 6 },
                name: "x".to_string(),
            }),
            clauses: vec![
                Clause {
                    meta: Meta { start: 9, end: 18 },
                    pattern: Pattern::Int {
                        meta: Meta { start: 11, end: 12 },
                        value: 1
                    },
                    then: Box::new(Expr::Int {
                        meta: Meta { start: 16, end: 18 },
                        typ: (),
                        value: 10
                    })
                },
                Clause {
                    meta: Meta { start: 19, end: 28 },
                    pattern: Pattern::Int {
                        meta: Meta { start: 21, end: 22 },
                        value: 2
                    },
                    then: Box::new(Expr::Int {
                        meta: Meta { start: 26, end: 28 },
                        typ: (),
                        value: 20
                    })
                }
            ]
        }),
        ExprParser::new().parse("case x { | 1 -> 10 | 2 -> 20 }"),
    );
}

#[test]
fn module_test() {
    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![]
        }),
        ModuleParser::new().parse(""),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::Import {
                meta: Meta { start: 0, end: 12 },
                module: "magic".to_string()
            }]
        }),
        ModuleParser::new().parse("import magic"),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::ExternalType {
                meta: Meta { start: 0, end: 18 },
                public: false,
                name: "Conn".to_string(),
                args: vec![],
            }]
        }),
        ModuleParser::new().parse("external type Conn"),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::ExternalType {
                meta: Meta { start: 0, end: 22 },
                public: true,
                name: "Conn".to_string(),
                args: vec![],
            }]
        }),
        ModuleParser::new().parse("pub external type Conn"),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::ExternalType {
                meta: Meta { start: 0, end: 26 },
                public: false,
                name: "Vector".to_string(),
                args: vec!["a".to_string(), "b".to_string()],
            }]
        }),
        ModuleParser::new().parse("external type Vector(a, b)"),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::Fn {
                meta: Meta { start: 0, end: 24 },
                public: false,
                name: "run".to_string(),
                args: vec![
                    Arg {
                        name: "one".to_string()
                    },
                    Arg {
                        name: "two".to_string()
                    }
                ],
                body: Expr::Seq {
                    meta: Meta { start: 19, end: 22 },
                    typ: (),
                    first: Box::new(Expr::Int {
                        typ: (),
                        meta: Meta { start: 19, end: 20 },
                        value: 1
                    }),
                    then: Box::new(Expr::Int {
                        typ: (),
                        meta: Meta { start: 21, end: 22 },
                        value: 2
                    })
                }
            }]
        }),
        ModuleParser::new().parse("fn run(one, two) { 1 2 }"),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::Fn {
                meta: Meta { start: 0, end: 17 },
                public: true,
                name: "go".to_string(),
                args: vec![],
                body: Expr::Int {
                    typ: (),
                    meta: Meta { start: 14, end: 15 },
                    value: 1
                },
            }]
        }),
        ModuleParser::new().parse("pub fn go() { 1 }"),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::Test {
                meta: Meta { start: 0, end: 16 },
                name: "run".to_string(),
                body: Expr::Seq {
                    meta: Meta { start: 11, end: 14 },
                    typ: (),
                    first: Box::new(Expr::Int {
                        typ: (),
                        meta: Meta { start: 11, end: 12 },
                        value: 1
                    }),
                    then: Box::new(Expr::Int {
                        typ: (),
                        meta: Meta { start: 13, end: 14 },
                        value: 2
                    })
                }
            }]
        }),
        ModuleParser::new().parse("test run { 1 2 }"),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::ExternalFn {
                meta: Meta { start: 0, end: 49 },
                name: "run".to_string(),
                module: "m".to_string(),
                fun: "f".to_string(),
                args: vec![
                    Type::Constructor {
                        meta: Meta { start: 20, end: 23 },
                        name: "Int".to_string(),
                        args: vec![]
                    },
                    Type::Constructor {
                        meta: Meta { start: 25, end: 30 },
                        name: "Float".to_string(),
                        args: vec![]
                    }
                ],
                public: true,
                retrn: Type::Constructor {
                    meta: Meta { start: 35, end: 39 },
                    name: "Bool".to_string(),
                    args: vec![]
                }
            }]
        }),
        ModuleParser::new().parse("pub external fn run(Int, Float) -> Bool = 'm' 'f'"),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::Enum {
                meta: Meta { start: 0, end: 26 },
                public: true,
                args: vec!["a".to_string()],
                constructors: vec![EnumConstructor {
                    meta: Meta { start: 18, end: 26 },
                    name: "Boxed".to_string(),
                    args: vec![Type::Var {
                        meta: Meta { start: 24, end: 25 },
                        name: "a".to_string()
                    }]
                }],
                name: "Box".to_string(),
            }]
        }),
        ModuleParser::new().parse("pub enum Box(a) = Boxed(a)"),
    );
}
