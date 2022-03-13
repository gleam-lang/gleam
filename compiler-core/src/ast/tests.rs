use crate::{
    ast::{SrcSpan, TypedExpr},
    type_,
};

#[test]
fn find_node_string() {
    let expr = TypedExpr::String {
        location: SrcSpan { start: 5, end: 8 },
        typ: type_::string(),
        value: "".into(),
    };

    assert_eq!(expr.find_node(3), None);
    assert_eq!(expr.find_node(5), Some(&expr));
    assert_eq!(expr.find_node(7), Some(&expr));
    assert_eq!(expr.find_node(8), None);
}

#[test]
fn find_node_float() {
    let expr = TypedExpr::Float {
        location: SrcSpan { start: 5, end: 8 },
        typ: type_::float(),
        value: "".into(),
    };

    assert_eq!(expr.find_node(3), None);
    assert_eq!(expr.find_node(5), Some(&expr));
    assert_eq!(expr.find_node(7), Some(&expr));
    assert_eq!(expr.find_node(8), None);
}

#[test]
fn find_node_int() {
    let expr = TypedExpr::Int {
        location: SrcSpan { start: 5, end: 8 },
        typ: type_::int(),
        value: "".into(),
    };

    assert_eq!(expr.find_node(3), None);
    assert_eq!(expr.find_node(5), Some(&expr));
    assert_eq!(expr.find_node(7), Some(&expr));
    assert_eq!(expr.find_node(8), None);
}

#[test]
fn find_node_var() {
    let expr = TypedExpr::Var {
        location: SrcSpan { start: 5, end: 8 },
        constructor: type_::ValueConstructor {
            public: true,
            origin: SrcSpan::default(),
            variant: type_::ValueConstructorVariant::LocalVariable,
            type_: type_::int(),
        },
        name: "wibble".into(),
    };

    assert_eq!(expr.find_node(3), None);
    assert_eq!(expr.find_node(5), Some(&expr));
    assert_eq!(expr.find_node(7), Some(&expr));
    assert_eq!(expr.find_node(8), None);
}
