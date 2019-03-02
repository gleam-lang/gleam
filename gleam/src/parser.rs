/// Blanks out comments, semicolons, etc
///
pub fn strip_extra(src: &str) -> String {
    enum Mode {
        Normal,
        String,
        Comment,
    };

    let mut buffer = String::with_capacity(src.len());
    let mut mode = Mode::Normal;
    let mut chars = src.chars();
    while let Some(c) = chars.next() {
        match mode {
            Mode::Normal => match c {
                ';' => buffer.push(' '),

                '"' => {
                    mode = Mode::String;
                    buffer.push(c);
                }

                '/' => match chars.next() {
                    Some('/') => {
                        mode = Mode::Comment;
                        buffer.push(' ');
                        buffer.push(' ');
                    }
                    Some(c2) => {
                        buffer.push(c);
                        buffer.push(c2);
                    }
                    None => buffer.push(c),
                },

                _ => buffer.push(c),
            },

            Mode::String => match c {
                '\\' => {
                    buffer.push(c);
                    chars.next().map(|c| buffer.push(c));
                }

                '"' => {
                    mode = Mode::Normal;
                    buffer.push(c);
                }

                _ => buffer.push(c),
            },

            Mode::Comment => match c {
                '\n' => {
                    mode = Mode::Normal;
                    buffer.push('\n');
                }
                _ => buffer.push(' '),
            },
        }
    }
    buffer
}

#[test]
fn strip_extra_test() {
    assert_eq!(strip_extra(&""), "".to_string());
    assert_eq!(strip_extra(&" ; "), "   ".to_string());
    assert_eq!(strip_extra(&" // hi\n "), "      \n ".to_string());
    assert_eq!(strip_extra(&r#""\"//" hi"#), r#""\"//" hi"#.to_string());
}

pub fn meta(start: usize, end: usize) -> crate::ast::Meta {
    crate::ast::Meta { start, end }
}

#[test]
fn expr_test() {
    use crate::ast::*;
    use crate::grammar::ExprParser;

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
            meta: Meta { start: 0, end: 14 },
            typ: (),
            value: Box::new(Expr::BinOp {
                meta: Meta { start: 8, end: 13 },
                typ: (),
                name: BinOp::AddInt,
                left: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 8, end: 9 },
                    value: 1
                }),
                right: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 12, end: 13 },
                    value: 2
                }),
            }),
            pattern: Pattern::Var {
                meta: Meta { start: 4, end: 5 },
                name: "x".to_string(),
            },
            then: Box::new(Expr::Var {
                typ: (),
                scope: (),
                meta: Meta { start: 14, end: 15 },
                name: "x".to_string(),
            })
        }),
        ExprParser::new().parse("let x = 1 + 2 x"),
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
            meta: Meta { start: 0, end: 10 },
            typ: (),
            value: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 8, end: 9 },
                value: 1
            }),
            pattern: Pattern::Var {
                meta: Meta { start: 4, end: 5 },
                name: "x".to_string(),
            },
            then: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 10, end: 11 },
                value: 2
            })
        }),
        ExprParser::new().parse("let x = 1 2"),
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
            is_capture: false,
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
            meta: Meta { start: 0, end: 10 },
            typ: (),
            is_capture: true,
            args: vec![Arg {
                name: Some("capture@1".to_string())
            }],
            body: Box::new(Expr::Call {
                meta: Meta { start: 0, end: 10 },
                typ: (),
                fun: Box::new(Expr::Var {
                    meta: Meta { start: 0, end: 1 },
                    typ: (),
                    scope: (),
                    name: "f".to_string(),
                }),
                args: vec![
                    Expr::Int {
                        meta: Meta { start: 2, end: 3 },
                        typ: (),
                        value: 1
                    },
                    Expr::Var {
                        meta: Meta { start: 5, end: 6 },
                        typ: (),
                        scope: (),
                        name: "capture@1".to_string(),
                    },
                    Expr::Int {
                        meta: Meta { start: 8, end: 9 },
                        typ: (),
                        value: 3
                    }
                ]
            })
        }),
        ExprParser::new().parse("f(1, _, 3)"),
    );

    assert_eq!(
        Ok(Expr::Fn {
            meta: Meta { start: 0, end: 16 },
            is_capture: false,
            typ: (),
            args: vec![
                Arg {
                    name: Some("a".to_string())
                },
                Arg {
                    name: Some("b".to_string())
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
            meta: Meta { start: 0, end: 10 },
            typ: (),
            value: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 8, end: 9 },
                value: 1
            }),
            pattern: Pattern::Int {
                meta: Meta { start: 4, end: 5 },
                value: 0,
            },
            then: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 10, end: 11 },
                value: 2
            })
        }),
        ExprParser::new().parse("let 0 = 1 2"),
    );

    assert_eq!(
        Ok(Expr::Let {
            meta: Meta { start: 0, end: 12 },
            typ: (),
            value: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 10, end: 11 },
                value: 1,
            }),
            pattern: Pattern::Float {
                meta: Meta { start: 4, end: 7 },
                value: 1.0,
            },
            then: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 12, end: 13 },
                value: 2
            })
        }),
        ExprParser::new().parse("let 1.0 = 1 2"),
    );

    assert_eq!(
        Ok(Expr::Let {
            meta: Meta { start: 0, end: 12 },
            typ: (),
            value: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 10, end: 11 },
                value: 1
            }),
            pattern: Pattern::String {
                meta: Meta { start: 4, end: 7 },
                value: "a".to_string(),
            },
            then: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 12, end: 13 },
                value: 2
            })
        }),
        ExprParser::new().parse("let \"a\" = 1 2"),
    );

    assert_eq!(
        Ok(Expr::Let {
            meta: Meta { start: 0, end: 23 },
            typ: (),
            pattern: Pattern::Constructor {
                meta: Meta { start: 4, end: 18 },
                module: Some("option".to_string()),
                name: "Some".to_string(),
                args: vec![Pattern::Var {
                    meta: Meta { start: 16, end: 17 },
                    name: "a".to_string()
                }]
            },
            value: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 21, end: 22 },
                value: 1
            }),
            then: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 23, end: 24 },
                value: 2
            })
        }),
        ExprParser::new().parse("let option:Some(a) = 1 2"),
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
    use crate::ast::*;
    use crate::grammar::ModuleParser;

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
                        name: Some("one".to_string())
                    },
                    Arg {
                        name: Some("two".to_string())
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
        ModuleParser::new().parse("pub external fn run(Int, Float) -> Bool = \"m\" \"f\""),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::Enum {
                meta: Meta { start: 0, end: 28 },
                public: true,
                args: vec!["a".to_string()],
                constructors: vec![EnumConstructor {
                    meta: Meta { start: 20, end: 28 },
                    name: "Boxed".to_string(),
                    args: vec![Type::Var {
                        meta: Meta { start: 26, end: 27 },
                        name: "a".to_string()
                    }]
                }],
                name: "Box".to_string(),
            }]
        }),
        ModuleParser::new().parse("pub enum Box(a) = | Boxed(a)"),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![
                Statement::Enum {
                    meta: Meta { start: 0, end: 28 },
                    public: true,
                    args: vec!["x".to_string()],
                    constructors: vec![EnumConstructor {
                        meta: Meta { start: 20, end: 28 },
                        name: "Boxxy".to_string(),
                        args: vec![Type::Var {
                            meta: Meta { start: 26, end: 27 },
                            name: "x".to_string()
                        }]
                    }],
                    name: "Box".to_string(),
                },
                Statement::Fn {
                    meta: Meta {
                        start: 43,
                        end: 124
                    },
                    public: true,
                    name: "value".to_string(),
                    args: vec![Arg {
                        name: Some("x".to_string())
                    }],
                    body: Expr::Let {
                        meta: Meta {
                            start: 76,
                            end: 108
                        },
                        typ: (),
                        value: Box::new(Expr::Var {
                            typ: (),
                            meta: Meta { start: 91, end: 92 },
                            scope: (),
                            name: "x".to_string()
                        }),
                        pattern: Pattern::Constructor {
                            meta: Meta { start: 80, end: 88 },
                            module: None,
                            name: "Boxxy".to_string(),
                            args: vec![Pattern::Var {
                                meta: Meta { start: 86, end: 87 },
                                name: "a".to_string()
                            }]
                        },
                        then: Box::new(Expr::Var {
                            meta: Meta {
                                start: 108,
                                end: 109
                            },
                            scope: (),
                            typ: (),
                            name: "a".to_string()
                        }),
                    }
                }
            ]
        }),
        ModuleParser::new().parse(
            "pub enum Box(x) = | Boxxy(x)

             pub fn value(x) {
               let Boxxy(a) = x
               a
             }"
        ),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::Fn {
                meta: Meta { start: 0, end: 75 },
                public: true,
                name: "value".to_string(),
                args: vec![Arg {
                    name: Some("x".to_string())
                }],
                body: Expr::Let {
                    meta: Meta { start: 33, end: 59 },
                    typ: (),
                    value: Box::new(Expr::Var {
                        typ: (),
                        meta: Meta { start: 42, end: 43 },
                        scope: (),
                        name: "x".to_string()
                    }),
                    pattern: Pattern::Nil {
                        meta: Meta { start: 37, end: 39 },
                    },
                    then: Box::new(Expr::Var {
                        meta: Meta { start: 59, end: 60 },
                        scope: (),
                        typ: (),
                        name: "a".to_string()
                    }),
                }
            }]
        }),
        ModuleParser::new().parse(
            "pub fn value(x) {
               let [] = x
               a
             }"
        ),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::Fn {
                meta: Meta { start: 0, end: 79 },
                public: true,
                name: "value".to_string(),
                args: vec![Arg {
                    name: Some("x".to_string())
                }],
                body: Expr::Let {
                    meta: Meta { start: 33, end: 63 },
                    typ: (),
                    value: Box::new(Expr::Var {
                        typ: (),
                        meta: Meta { start: 46, end: 47 },
                        scope: (),
                        name: "x".to_string()
                    }),
                    pattern: Pattern::Cons {
                        meta: Meta { start: 38, end: 39 },
                        head: Box::new(Pattern::Var {
                            meta: Meta { start: 38, end: 39 },
                            name: "a".to_string()
                        }),
                        tail: Box::new(Pattern::Cons {
                            meta: Meta { start: 41, end: 42 },
                            head: Box::new(Pattern::Var {
                                meta: Meta { start: 41, end: 42 },
                                name: "b".to_string()
                            }),
                            tail: Box::new(Pattern::Nil {
                                meta: Meta { start: 42, end: 43 },
                            })
                        })
                    },
                    then: Box::new(Expr::Var {
                        meta: Meta { start: 63, end: 64 },
                        scope: (),
                        typ: (),
                        name: "a".to_string()
                    }),
                }
            }]
        }),
        ModuleParser::new().parse(
            "pub fn value(x) {
               let [a, b] = x
               a
             }"
        ),
    );

    assert_eq!(
        Ok(Module {
            typ: (),
            name: "".to_string(),
            statements: vec![Statement::Fn {
                meta: Meta { start: 0, end: 87 },
                public: true,
                name: "value".to_string(),
                args: vec![Arg {
                    name: Some("x".to_string())
                }],
                body: Expr::Let {
                    meta: Meta { start: 33, end: 71 },
                    typ: (),
                    value: Box::new(Expr::Var {
                        typ: (),
                        meta: Meta { start: 54, end: 55 },
                        scope: (),
                        name: "x".to_string()
                    }),
                    pattern: Pattern::Cons {
                        meta: Meta { start: 37, end: 51 },
                        head: Box::new(Pattern::Var {
                            meta: Meta { start: 38, end: 39 },
                            name: "a".to_string()
                        }),
                        tail: Box::new(Pattern::Cons {
                            meta: Meta { start: 42, end: 50 },
                            head: Box::new(Pattern::Var {
                                meta: Meta { start: 43, end: 44 },
                                name: "b".to_string()
                            }),
                            tail: Box::new(Pattern::Nil {
                                meta: Meta { start: 47, end: 49 },
                            })
                        })
                    },
                    then: Box::new(Expr::Var {
                        meta: Meta { start: 71, end: 72 },
                        scope: (),
                        typ: (),
                        name: "a".to_string()
                    }),
                }
            }]
        }),
        ModuleParser::new().parse(
            "pub fn value(x) {
               let [a | [b | []]] = x
               a
             }"
        ),
    );
}
