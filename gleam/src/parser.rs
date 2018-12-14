use crate::ast::{Clause, Expr, Meta, Scope};
use crate::grammar::ExprParser;
use crate::pattern::Pattern;

pub fn meta(start: usize, end: usize) -> Meta {
    Meta { start, end }
}

#[test]
fn expr_test() {
    assert_eq!(
        Ok(Expr::Int {
            meta: Meta { start: 0, end: 3 },
            value: 123
        }),
        ExprParser::new().parse("123"),
    );

    assert_eq!(
        Ok(Expr::Int {
            meta: Meta { start: 0, end: 3 },
            value: -45
        }),
        ExprParser::new().parse("-45"),
    );

    assert_eq!(
        Ok(Expr::Float {
            meta: Meta { start: 0, end: 3 },
            value: -1.0
        }),
        ExprParser::new().parse("-1."),
    );

    assert_eq!(
        Ok(Expr::Float {
            meta: Meta { start: 0, end: 4 },
            value: 1.23
        }),
        ExprParser::new().parse("1.23"),
    );

    assert_eq!(
        Ok(Expr::Float {
            meta: Meta { start: 0, end: 5 },
            value: -1.23
        }),
        ExprParser::new().parse("-1.23"),
    );

    assert_eq!(
        Ok(Expr::String {
            meta: Meta { start: 0, end: 15 },
            value: "Hello, world!".to_string(),
        }),
        ExprParser::new().parse(r#""Hello, world!""#),
    );

    assert_eq!(
        Ok(Expr::String {
            meta: Meta { start: 0, end: 13 },
            value: "quote -> \\\"".to_string(),
        }),
        ExprParser::new().parse(r#""quote -> \"""#),
    );

    assert_eq!(
        Ok(Expr::Atom {
            meta: Meta { start: 0, end: 15 },
            value: "Hello, world!".to_string(),
        }),
        ExprParser::new().parse(r#"'Hello, world!'"#),
    );

    assert_eq!(
        Ok(Expr::String {
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
                scope: Scope::Local,
            }),
            args: vec![
                Expr::Int {
                    meta: Meta { start: 6, end: 7 },
                    value: 1
                },
                Expr::Int {
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
            scope: Scope::Local,
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
                    meta: Meta { start: 5, end: 6 },
                    value: 1
                },
                Expr::Int {
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
        Ok(Expr::Let {
            meta: Meta { start: 0, end: 6 },
            typ: (),
            value: Box::new(Expr::Int {
                meta: Meta { start: 4, end: 5 },
                value: 1
            }),
            clause: Clause {
                typ: (),
                meta: Meta { start: 0, end: 6 },
                pattern: Pattern::Var {
                    meta: Meta { start: 0, end: 1 },
                    name: "x".to_string(),
                },
                then: Box::new(Expr::Int {
                    meta: Meta { start: 6, end: 7 },
                    value: 2
                })
            }
        }),
        ExprParser::new().parse("x = 1 2"),
    );

    assert_eq!(
        Ok(Expr::Seq {
            meta: Meta { start: 0, end: 3 },
            first: Box::new(Expr::Int {
                meta: Meta { start: 0, end: 1 },
                value: 1
            }),
            then: Box::new(Expr::Int {
                meta: Meta { start: 2, end: 3 },
                value: 2
            }),
        }),
        ExprParser::new().parse("1 2"),
    );

    assert_eq!(
        Ok(Expr::Tuple {
            meta: Meta { start: 0, end: 9 },
            typ: (),
            elems: vec![
                Expr::Int {
                    meta: Meta { start: 1, end: 2 },
                    value: 1
                },
                Expr::Int {
                    meta: Meta { start: 4, end: 5 },
                    value: 2
                },
                Expr::Int {
                    meta: Meta { start: 7, end: 8 },
                    value: 3
                },
            ]
        }),
        ExprParser::new().parse("{1, 2, 3}"),
    );
}
