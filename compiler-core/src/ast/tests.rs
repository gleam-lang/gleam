use crate::{
    ast::{SrcSpan, TypedExpr},
    type_::{self, Environment, ExprTyper, ValueConstructor, ValueConstructorVariant},
    uid::UniqueIdGenerator,
};

fn compile_expression(src: &str) -> TypedExpr {
    let ast = crate::parse::parse_expression_sequence(src).expect("syntax error");

    let mut modules = im::HashMap::new();
    let ids = UniqueIdGenerator::new();
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = modules.insert("gleam".to_string(), type_::build_prelude(&ids));
    ExprTyper::new(&mut Environment::new(ids, &[], &modules, &mut vec![]))
        .infer(ast)
        .expect("should successfully infer")
}

#[test]
fn find_node_string() {
    let expr = compile_expression(r#" "ok" "#);
    assert_eq!(expr.find_node(0), None);
    assert_eq!(expr.find_node(1), Some(&expr));
    assert_eq!(expr.find_node(4), Some(&expr));
    assert_eq!(expr.find_node(5), None);
}

#[test]
fn find_node_float() {
    let expr = compile_expression(r#" 1.02 "#);
    assert_eq!(expr.find_node(0), None);
    assert_eq!(expr.find_node(1), Some(&expr));
    assert_eq!(expr.find_node(4), Some(&expr));
    assert_eq!(expr.find_node(5), None);
}

#[test]
fn find_node_int() {
    let expr = compile_expression(r#" 1302 "#);
    assert_eq!(expr.find_node(0), None);
    assert_eq!(expr.find_node(1), Some(&expr));
    assert_eq!(expr.find_node(4), Some(&expr));
    assert_eq!(expr.find_node(5), None);
}

#[test]
fn find_node_var() {
    let expr = compile_expression(
        r#"let wibble = 1
wibble"#,
    );

    let var = TypedExpr::Var {
        location: SrcSpan { start: 15, end: 21 },
        constructor: ValueConstructor {
            public: false,
            origin: SrcSpan { start: 4, end: 10 },
            variant: ValueConstructorVariant::LocalVariable,
            type_: type_::int(),
        },
        name: "wibble".into(),
    };

    assert_eq!(expr.find_node(14), None);
    assert_eq!(expr.find_node(15), Some(&var));
    assert_eq!(expr.find_node(20), Some(&var));
    assert_eq!(expr.find_node(21), None);
}

#[test]
fn find_node_sequence() {
    let expr = compile_expression(r#"1 2 3"#);

    assert!(expr.find_node(0).is_some());
    assert!(expr.find_node(1).is_none());
    assert!(expr.find_node(2).is_some());
    assert!(expr.find_node(3).is_none());
    assert!(expr.find_node(4).is_some());
    assert!(expr.find_node(5).is_none());
}
