use crate::ast::{Expr, Meta};
use crate::grammar::ExprParser;

pub fn meta(start: usize, end: usize) -> Meta {
    Meta { start, end }
}

#[test]
fn expr_test() {
    assert_eq!(
        ExprParser::new().parse("123"),
        Ok(Expr::Int {
            meta: Meta { start: 0, end: 3 },
            value: 123
        })
    );

    assert_eq!(
        ExprParser::new().parse("-45"),
        Ok(Expr::Int {
            meta: Meta { start: 0, end: 3 },
            value: -45
        })
    );

    assert_eq!(
        ExprParser::new().parse("-1."),
        Ok(Expr::Float {
            meta: Meta { start: 0, end: 3 },
            value: -1.0
        })
    );

    assert_eq!(
        ExprParser::new().parse("1.23"),
        Ok(Expr::Float {
            meta: Meta { start: 0, end: 4 },
            value: 1.23
        })
    );

    assert_eq!(
        ExprParser::new().parse("-1.23"),
        Ok(Expr::Float {
            meta: Meta { start: 0, end: 5 },
            value: -1.23
        })
    );

    assert_eq!(
        ExprParser::new().parse("{1, 2, 3}"),
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
        })
    );
}
