#[derive(Debug, PartialEq)]
pub enum Error {
    TooManyHolesInCapture {
        meta: crate::ast::Meta,
        count: usize,
    },
}

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
                    if let Some(c) = chars.next() {
                        buffer.push(c)
                    }
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

pub fn seq(mut exprs: Vec<crate::ast::UntypedExpr>) -> crate::ast::UntypedExpr {
    use crate::ast::*;

    let head = exprs.pop().unwrap();
    exprs.into_iter().rev().fold(head, |acc, expr| Expr::Seq {
        typ: (),
        first: Box::new(expr),
        then: Box::new(acc),
    })
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
        Ok(Expr::Call {
            meta: Meta { start: 0, end: 11 },
            typ: (),
            fun: Box::new(Expr::Var {
                meta: Meta { start: 0, end: 5 },
                name: "hello".to_string(),
                constructor: (),
            }),
            args: vec![
                CallArg {
                    meta: Meta { start: 6, end: 7 },
                    label: None,
                    value: Expr::Int {
                        typ: (),
                        meta: Meta { start: 6, end: 7 },
                        value: 1
                    }
                },
                CallArg {
                    label: None,
                    meta: Meta { start: 9, end: 10 },
                    value: Expr::Int {
                        typ: (),
                        meta: Meta { start: 9, end: 10 },
                        value: 2
                    }
                },
            ]
        }),
        ExprParser::new().parse("hello(1, 2)"),
    );

    assert_eq!(
        Ok(Expr::Var {
            meta: Meta { start: 0, end: 5 },
            name: "hello".to_string(),
            constructor: (),
        }),
        ExprParser::new().parse("hello"),
    );

    assert_eq!(
        Ok(Expr::Call {
            meta: Meta { start: 0, end: 10 },
            typ: (),
            fun: Box::new(Expr::Var {
                meta: Meta { start: 0, end: 4 },
                constructor: (),
                name: "Pair".to_string(),
            }),
            args: vec![
                CallArg {
                    label: None,
                    meta: Meta { start: 5, end: 6 },
                    value: Expr::Int {
                        typ: (),
                        meta: Meta { start: 5, end: 6 },
                        value: 1
                    }
                },
                CallArg {
                    label: None,
                    meta: Meta { start: 8, end: 9 },
                    value: Expr::Int {
                        typ: (),
                        meta: Meta { start: 8, end: 9 },
                        value: 3
                    }
                }
            ]
        }),
        ExprParser::new().parse("Pair(1, 3)"),
    );

    assert_eq!(
        Ok(Expr::Call {
            meta: Meta { start: 0, end: 17 },
            typ: (),
            fun: Box::new(Expr::Var {
                meta: Meta { start: 0, end: 5 },
                constructor: (),
                name: "Point".to_string(),
            }),
            args: vec![
                CallArg {
                    label: Some("x".to_string()),
                    meta: Meta { start: 6, end: 10 },
                    value: Expr::Int {
                        typ: (),
                        meta: Meta { start: 9, end: 10 },
                        value: 1
                    }
                },
                CallArg {
                    label: Some("y".to_string()),
                    meta: Meta { start: 12, end: 16 },
                    value: Expr::Int {
                        typ: (),
                        meta: Meta { start: 15, end: 16 },
                        value: 3
                    }
                }
            ]
        }),
        ExprParser::new().parse("Point(x: 1, y: 3)"),
    );

    assert_eq!(
        Ok(Expr::Call {
            meta: Meta { start: 0, end: 14 },
            typ: (),
            fun: Box::new(Expr::Var {
                meta: Meta { start: 0, end: 5 },
                constructor: (),
                name: "Point".to_string(),
            }),
            args: vec![
                CallArg {
                    label: None,
                    meta: Meta { start: 6, end: 7 },
                    value: Expr::Int {
                        typ: (),
                        meta: Meta { start: 6, end: 7 },
                        value: 1
                    }
                },
                CallArg {
                    label: Some("y".to_string()),
                    meta: Meta { start: 9, end: 13 },
                    value: Expr::Int {
                        typ: (),
                        meta: Meta { start: 12, end: 13 },
                        value: 3
                    }
                }
            ]
        }),
        ExprParser::new().parse("Point(1, y: 3)"),
    );

    assert_eq!(
        Ok(Expr::Cons {
            meta: Meta { start: 5, end: 7 },
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
            meta: Meta { start: 0, end: 3 },
            typ: (),
            head: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 1, end: 2 },
                value: 1
            }),
            tail: Box::new(Expr::Nil {
                meta: Meta { start: 0, end: 3 },
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
                constructor: (),
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
        Ok(Expr::BinOp {
            meta: Meta { start: 0, end: 5 },
            typ: (),
            name: BinOp::GtInt,
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
        ExprParser::new().parse("1 > 2"),
    );

    assert_eq!(
        Ok(Expr::BinOp {
            meta: Meta { start: 0, end: 9 },
            typ: (),
            name: BinOp::GtFloat,
            left: Box::new(Expr::Float {
                typ: (),
                meta: Meta { start: 0, end: 2 },
                value: 1.0
            }),
            right: Box::new(Expr::Float {
                typ: (),
                meta: Meta { start: 6, end: 9 },
                value: 2.3
            }),
        }),
        ExprParser::new().parse("1. >. 2.3"),
    );

    assert_eq!(
        Ok(Expr::Cons {
            meta: Meta { start: 0, end: 6 },
            typ: (),
            head: Box::new(Expr::Int {
                typ: (),
                meta: Meta { start: 1, end: 2 },
                value: 1
            }),
            tail: Box::new(Expr::Cons {
                meta: Meta { start: 0, end: 6 },
                typ: (),
                head: Box::new(Expr::Int {
                    typ: (),
                    meta: Meta { start: 4, end: 5 },
                    value: 2
                }),
                tail: Box::new(Expr::Nil {
                    meta: Meta { start: 0, end: 6 },
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
        Ok(Expr::FieldSelect {
            meta: Meta { start: 0, end: 11 },
            typ: (),
            label: "name".to_string(),
            container: Box::new(Expr::Var {
                meta: Meta { start: 0, end: 6 },
                name: "person".to_string(),
                constructor: (),
            })
        }),
        ExprParser::new().parse("person.name"),
    );

    assert_eq!(
        Ok(Expr::Tuple {
            meta: Meta { start: 0, end: 14 },
            typ: (),
            elems: vec![
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
                Expr::Int {
                    typ: (),
                    meta: Meta { start: 12, end: 13 },
                    value: 3
                },
            ]
        }),
        ExprParser::new().parse("tuple(1, 2, 3)"),
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
                meta: Meta { start: 0, end: 0 },
                annotation: None,
                names: ArgNames::Named {
                    name: "capture@1".to_string()
                }
            }],
            body: Box::new(Expr::Call {
                meta: Meta { start: 0, end: 10 },
                typ: (),
                fun: Box::new(Expr::Var {
                    meta: Meta { start: 0, end: 1 },
                    constructor: (),
                    name: "f".to_string(),
                }),
                args: vec![
                    CallArg {
                        label: None,
                        meta: Meta { start: 2, end: 3 },
                        value: Expr::Int {
                            meta: Meta { start: 2, end: 3 },
                            typ: (),
                            value: 1
                        }
                    },
                    CallArg {
                        label: None,
                        meta: Meta { start: 0, end: 0 },
                        value: Expr::Var {
                            meta: Meta { start: 5, end: 6 },
                            constructor: (),
                            name: "capture@1".to_string(),
                        },
                    },
                    CallArg {
                        label: None,
                        meta: Meta { start: 8, end: 9 },
                        value: Expr::Int {
                            meta: Meta { start: 8, end: 9 },
                            typ: (),
                            value: 3
                        }
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
                    meta: Meta { start: 3, end: 4 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "a".to_string()
                    }
                },
                Arg {
                    meta: Meta { start: 6, end: 7 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "b".to_string()
                    }
                },
            ],
            body: Box::new(Expr::Seq {
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
                module: None,
                name: "Some".to_string(),
                args: vec![CallArg {
                    label: Some("body".to_string()),
                    meta: Meta { start: 9, end: 17 },
                    value: Pattern::Var {
                        meta: Meta { start: 16, end: 17 },
                        name: "a".to_string()
                    }
                }],
                constructor: (),
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
        ExprParser::new().parse("let Some(body:  a) = 1 2"),
    );

    assert_eq!(
        Ok(Expr::Let {
            meta: Meta { start: 0, end: 23 },
            typ: (),
            pattern: Pattern::Constructor {
                meta: Meta { start: 4, end: 18 },
                module: Some("option".to_string()),
                name: "Some".to_string(),
                args: vec![CallArg {
                    label: None,
                    meta: Meta { start: 16, end: 17 },
                    value: Pattern::Var {
                        meta: Meta { start: 16, end: 17 },
                        name: "a".to_string()
                    }
                }],
                constructor: (),
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
        ExprParser::new().parse("let option.Some(a) = 1 2"),
    );

    assert_eq!(
        Ok(Expr::Case {
            meta: Meta { start: 0, end: 26 },
            typ: (),
            subjects: vec![Expr::Var {
                constructor: (),
                meta: Meta { start: 5, end: 6 },
                name: "x".to_string(),
            }],
            clauses: vec![
                Clause {
                    meta: Meta { start: 9, end: 16 },
                    patterns: vec![Pattern::Int {
                        meta: Meta { start: 9, end: 10 },
                        value: 1
                    }],
                    then: Expr::Int {
                        meta: Meta { start: 14, end: 16 },
                        typ: (),
                        value: 10
                    }
                },
                Clause {
                    meta: Meta { start: 17, end: 24 },
                    patterns: vec![Pattern::Int {
                        meta: Meta { start: 17, end: 18 },
                        value: 2
                    }],
                    then: Expr::Int {
                        meta: Meta { start: 22, end: 24 },
                        typ: (),
                        value: 20
                    }
                }
            ]
        }),
        ExprParser::new().parse("case x { 1 -> 10 2 -> 20 }"),
    );
}

#[test]
fn module_test() {
    use crate::ast::*;
    use crate::grammar::ModuleParser;

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![]
        }),
        ModuleParser::new().parse(""),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![Statement::Import {
                meta: Meta { start: 7, end: 12 },
                module: vec!["magic".to_string()],
                unqualified: vec![],
                as_name: None,
            }]
        }),
        ModuleParser::new().parse("import magic"),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
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
            type_info: (),
            name: vec![],
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
            type_info: (),
            name: vec![],
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
            type_info: (),
            name: vec![],
            statements: vec![Statement::Fn {
                return_annotation: None,
                meta: Meta { start: 0, end: 24 },
                public: false,
                name: "run".to_string(),
                args: vec![
                    Arg {
                        meta: Meta { start: 7, end: 10 },
                        annotation: None,
                        names: ArgNames::Named {
                            name: "one".to_string()
                        }
                    },
                    Arg {
                        meta: Meta { start: 12, end: 15 },
                        annotation: None,
                        names: ArgNames::Named {
                            name: "two".to_string()
                        }
                    }
                ],
                body: Expr::Seq {
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
            type_info: (),
            name: vec![],
            statements: vec![Statement::Fn {
                return_annotation: None,
                meta: Meta { start: 0, end: 20 },
                public: false,
                name: "run".to_string(),
                args: vec![],
                body: Expr::Seq {
                    typ: (),
                    first: Box::new(Expr::Int {
                        typ: (),
                        meta: Meta { start: 11, end: 12 },
                        value: 1
                    }),
                    then: Box::new(Expr::Seq {
                        typ: (),
                        first: Box::new(Expr::Int {
                            typ: (),
                            meta: Meta { start: 13, end: 14 },
                            value: 2
                        }),
                        then: Box::new(Expr::Seq {
                            typ: (),
                            first: Box::new(Expr::Int {
                                typ: (),
                                meta: Meta { start: 15, end: 16 },
                                value: 3
                            }),
                            then: Box::new(Expr::Int {
                                typ: (),
                                meta: Meta { start: 17, end: 18 },
                                value: 4
                            })
                        })
                    })
                }
            }]
        }),
        ModuleParser::new().parse("fn run() { 1 2 3 4 }"),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![Statement::Fn {
                return_annotation: None,
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
            type_info: (),
            name: vec![],
            statements: vec![Statement::ExternalFn {
                meta: Meta { start: 0, end: 52 },
                name: "run".to_string(),
                module: "m".to_string(),
                fun: "f".to_string(),
                args: vec![
                    ExternalFnArg {
                        label: None,
                        typ: TypeAst::Constructor {
                            meta: Meta { start: 20, end: 23 },
                            module: None,
                            name: "Int".to_string(),
                            args: vec![]
                        },
                    },
                    ExternalFnArg {
                        label: Some("b".to_string()),
                        typ: TypeAst::Constructor {
                            meta: Meta { start: 28, end: 33 },
                            module: None,
                            name: "Float".to_string(),
                            args: vec![]
                        }
                    },
                ],
                public: true,
                retrn: TypeAst::Constructor {
                    meta: Meta { start: 38, end: 42 },
                    module: None,
                    name: "Bool".to_string(),
                    args: vec![]
                }
            }]
        }),
        ModuleParser::new().parse("pub external fn run(Int, b: Float) -> Bool = \"m\" \"f\""),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![Statement::CustomType {
                meta: Meta { start: 0, end: 16 },
                public: true,
                args: vec!["a".to_string()],
                constructors: vec![RecordConstructor {
                    meta: Meta { start: 18, end: 26 },
                    name: "Boxed".to_string(),
                    args: vec![(
                        None,
                        TypeAst::Var {
                            meta: Meta { start: 24, end: 25 },
                            name: "a".to_string()
                        }
                    )]
                }],
                name: "Box".to_string(),
            }]
        }),
        ModuleParser::new().parse("pub type Box(a) { Boxed(a) }"),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![
                Statement::CustomType {
                    meta: Meta { start: 0, end: 27 },
                    public: true,
                    args: vec!["x".to_string()],
                    constructors: vec![RecordConstructor {
                        meta: Meta { start: 29, end: 48 },
                        name: "Boxxy0123456789x".to_string(),
                        args: vec![(
                            None,
                            TypeAst::Var {
                                meta: Meta { start: 46, end: 47 },
                                name: "x".to_string()
                            }
                        )]
                    }],
                    name: "Box0123456789x".to_string(),
                },
                Statement::Fn {
                    return_annotation: None,
                    meta: Meta {
                        start: 65,
                        end: 157
                    },
                    public: true,
                    name: "value".to_string(),
                    args: vec![Arg {
                        meta: Meta { start: 78, end: 79 },
                        annotation: None,
                        names: ArgNames::Named {
                            name: "x".to_string()
                        }
                    }],
                    body: Expr::Let {
                        meta: Meta {
                            start: 98,
                            end: 141
                        },
                        typ: (),
                        value: Box::new(Expr::Var {
                            meta: Meta {
                                start: 124,
                                end: 125
                            },
                            constructor: (),
                            name: "x".to_string()
                        }),
                        pattern: Pattern::Constructor {
                            meta: Meta {
                                start: 102,
                                end: 121
                            },
                            constructor: (),
                            module: None,
                            name: "Boxxy0123456789x".to_string(),
                            args: vec![CallArg {
                                meta: Meta {
                                    start: 119,
                                    end: 120
                                },
                                label: None,
                                value: Pattern::Var {
                                    meta: Meta {
                                        start: 119,
                                        end: 120
                                    },
                                    name: "a".to_string()
                                }
                            }]
                        },
                        then: Box::new(Expr::Var {
                            meta: Meta {
                                start: 141,
                                end: 142
                            },
                            constructor: (),
                            name: "a".to_string()
                        }),
                    }
                }
            ]
        }),
        ModuleParser::new().parse(
            "pub type Box0123456789x(x) { Boxxy0123456789x(x) }

             pub fn value(x) {
               let Boxxy0123456789x(a) = x
               a
             }"
        ),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![
                Statement::CustomType {
                    meta: Meta { start: 0, end: 16 },
                    public: true,
                    args: vec!["x".to_string()],
                    constructors: vec![RecordConstructor {
                        meta: Meta { start: 18, end: 26 },
                        name: "Boxxy".to_string(),
                        args: vec![(
                            None,
                            TypeAst::Var {
                                meta: Meta { start: 24, end: 25 },
                                name: "x".to_string()
                            }
                        )]
                    }],
                    name: "Box".to_string(),
                },
                Statement::Fn {
                    return_annotation: None,
                    meta: Meta {
                        start: 43,
                        end: 124
                    },
                    public: true,
                    name: "value".to_string(),
                    args: vec![Arg {
                        meta: Meta { start: 56, end: 57 },
                        annotation: None,
                        names: ArgNames::Named {
                            name: "x".to_string()
                        }
                    }],
                    body: Expr::Let {
                        meta: Meta {
                            start: 76,
                            end: 108
                        },
                        typ: (),
                        value: Box::new(Expr::Var {
                            meta: Meta { start: 91, end: 92 },
                            constructor: (),
                            name: "x".to_string()
                        }),
                        pattern: Pattern::Constructor {
                            meta: Meta { start: 80, end: 88 },
                            constructor: (),
                            module: None,
                            name: "Boxxy".to_string(),
                            args: vec![CallArg {
                                label: None,
                                meta: Meta { start: 86, end: 87 },
                                value: Pattern::Var {
                                    meta: Meta { start: 86, end: 87 },
                                    name: "a".to_string()
                                }
                            }]
                        },
                        then: Box::new(Expr::Var {
                            meta: Meta {
                                start: 108,
                                end: 109
                            },
                            constructor: (),
                            name: "a".to_string()
                        }),
                    }
                }
            ]
        }),
        ModuleParser::new().parse(
            "pub type Box(x) { Boxxy(x) }

             pub fn value(x) {
               let Boxxy(a) = x
               a
             }"
        ),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![Statement::Fn {
                return_annotation: None,
                meta: Meta { start: 0, end: 75 },
                public: true,
                name: "value".to_string(),
                args: vec![Arg {
                    meta: Meta { start: 13, end: 14 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "x".to_string()
                    }
                }],
                body: Expr::Let {
                    meta: Meta { start: 33, end: 59 },
                    typ: (),
                    value: Box::new(Expr::Var {
                        constructor: (),
                        meta: Meta { start: 42, end: 43 },
                        name: "x".to_string()
                    }),
                    pattern: Pattern::Nil {
                        meta: Meta { start: 37, end: 39 },
                    },
                    then: Box::new(Expr::Var {
                        meta: Meta { start: 59, end: 60 },
                        constructor: (),
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
            type_info: (),
            name: vec![],
            statements: vec![Statement::Fn {
                return_annotation: None,
                meta: Meta { start: 0, end: 79 },
                public: true,
                name: "value".to_string(),
                args: vec![Arg {
                    meta: Meta { start: 13, end: 14 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "x".to_string()
                    }
                }],
                body: Expr::Let {
                    meta: Meta { start: 33, end: 63 },
                    typ: (),
                    value: Box::new(Expr::Var {
                        meta: Meta { start: 46, end: 47 },
                        constructor: (),
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
                        constructor: (),
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
            type_info: (),
            name: vec![],
            statements: vec![Statement::Fn {
                return_annotation: None,
                meta: Meta { start: 0, end: 88 },
                public: true,
                name: "value1".to_string(),
                args: vec![Arg {
                    meta: Meta { start: 14, end: 15 },
                    annotation: None,
                    names: ArgNames::Named {
                        name: "x".to_string()
                    }
                }],
                body: Expr::Let {
                    meta: Meta { start: 34, end: 72 },
                    typ: (),
                    value: Box::new(Expr::Var {
                        meta: Meta { start: 55, end: 56 },
                        constructor: (),
                        name: "x".to_string()
                    }),
                    pattern: Pattern::Cons {
                        meta: Meta { start: 38, end: 52 },
                        head: Box::new(Pattern::Var {
                            meta: Meta { start: 39, end: 40 },
                            name: "a".to_string()
                        }),
                        tail: Box::new(Pattern::Cons {
                            meta: Meta { start: 43, end: 51 },
                            head: Box::new(Pattern::Var {
                                meta: Meta { start: 44, end: 45 },
                                name: "b".to_string()
                            }),
                            tail: Box::new(Pattern::Nil {
                                meta: Meta { start: 48, end: 50 },
                            })
                        })
                    },
                    then: Box::new(Expr::Var {
                        meta: Meta { start: 72, end: 73 },
                        constructor: (),
                        name: "a".to_string()
                    }),
                }
            }]
        }),
        ModuleParser::new().parse(
            "pub fn value1(x) {
               let [a | [b | []]] = x
               a
             }"
        ),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![Statement::Import {
                meta: Meta { start: 7, end: 20 },
                module: vec!["one".to_string(), "two".to_string(), "three".to_string(),],
                unqualified: vec![],
                as_name: None,
            }]
        }),
        ModuleParser::new().parse("import one/two/three"),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![Statement::Fn {
                return_annotation: Some(TypeAst::Constructor {
                    args: vec![],
                    meta: Meta { start: 18, end: 23 },
                    module: None,
                    name: "Float".to_string()
                }),
                meta: Meta { start: 0, end: 31 },
                public: false,
                name: "run".to_string(),
                body: Expr::Float {
                    typ: (),
                    meta: Meta { start: 26, end: 29 },
                    value: 1.0
                },
                args: vec![Arg {
                    meta: Meta { start: 7, end: 13 },
                    names: ArgNames::Named {
                        name: "x".to_string()
                    },
                    annotation: Some(TypeAst::Constructor {
                        args: vec![],
                        meta: Meta { start: 10, end: 13 },
                        module: None,
                        name: "Int".to_string()
                    })
                }]
            }]
        }),
        ModuleParser::new().parse("fn run(x: Int) -> Float { 1.0 }"),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![Statement::Import {
                meta: Meta { start: 7, end: 21 },
                unqualified: vec![],
                module: vec!["one".to_string(), "two".to_string(), "three".to_string(),],
                as_name: Some("something".to_string()),
            }]
        }),
        ModuleParser::new().parse("import one/two/three as something"),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![Statement::TypeAlias {
                meta: Meta { start: 0, end: 27 },
                public: false,
                alias: "IntMap".to_string(),
                args: vec![],
                resolved_type: TypeAst::Constructor {
                    meta: Meta { start: 14, end: 27 },
                    module: None,
                    name: "Map".to_string(),
                    args: vec![
                        TypeAst::Constructor {
                            meta: Meta { start: 18, end: 21 },
                            module: None,
                            name: "Int".to_string(),
                            args: vec![],
                        },
                        TypeAst::Constructor {
                            meta: Meta { start: 23, end: 26 },
                            module: None,
                            name: "Int".to_string(),
                            args: vec![],
                        },
                    ]
                }
            }]
        }),
        ModuleParser::new().parse("type IntMap = Map(Int, Int)"),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![Statement::TypeAlias {
                meta: Meta { start: 0, end: 31 },
                public: true,
                alias: "IntMap".to_string(),
                args: vec![],
                resolved_type: TypeAst::Constructor {
                    meta: Meta { start: 18, end: 31 },
                    module: None,
                    name: "Map".to_string(),
                    args: vec![
                        TypeAst::Constructor {
                            meta: Meta { start: 22, end: 25 },
                            module: None,
                            name: "Int".to_string(),
                            args: vec![],
                        },
                        TypeAst::Constructor {
                            meta: Meta { start: 27, end: 30 },
                            module: None,
                            name: "Int".to_string(),
                            args: vec![],
                        },
                    ]
                }
            }]
        }),
        ModuleParser::new().parse("pub type IntMap = Map(Int, Int)"),
    );

    assert_eq!(
        Ok(Module {
            type_info: (),
            name: vec![],
            statements: vec![Statement::TypeAlias {
                meta: Meta { start: 0, end: 38 },
                public: true,
                alias: "Option".to_string(),
                args: vec!["a".to_string()],
                resolved_type: TypeAst::Constructor {
                    meta: Meta { start: 21, end: 38 },
                    module: None,
                    name: "Result".to_string(),
                    args: vec![
                        TypeAst::Var {
                            meta: Meta { start: 28, end: 29 },
                            name: "a".to_string(),
                        },
                        TypeAst::Constructor {
                            meta: Meta { start: 31, end: 37 },
                            module: None,
                            name: "String".to_string(),
                            args: vec![],
                        },
                    ]
                }
            }]
        }),
        ModuleParser::new().parse("pub type Option(a) = Result(a, String)"),
    );
}
