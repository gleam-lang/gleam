use crate::{
    ast::{SrcSpan, TypedExpr},
    type_::{
        self, Environment, ExprTyper, ModuleValueConstructor, ValueConstructor,
        ValueConstructorVariant,
    },
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
fn find_node_todo() {
    let expr = compile_expression(r#" todo "#);
    assert_eq!(expr.find_node(0), None);
    assert_eq!(expr.find_node(1), Some(&expr));
    assert_eq!(expr.find_node(4), Some(&expr));
    assert_eq!(expr.find_node(5), None);
}

#[test]
fn find_node_todo_with_string() {
    let expr = compile_expression(r#" todo("ok") "#);
    assert_eq!(expr.find_node(0), None);
    assert_eq!(expr.find_node(1), Some(&expr));
    assert_eq!(expr.find_node(10), Some(&expr));
    assert_eq!(expr.find_node(11), None);
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

#[test]
fn find_node_list() {
    let expr = compile_expression(r#"[1, 2, 3]"#);
    assert!(expr.find_node(0).is_none());
    assert!(expr.find_node(1).is_some());
    assert!(expr.find_node(2).is_none());
    assert!(expr.find_node(3).is_none());
    assert!(expr.find_node(4).is_some());
    assert!(expr.find_node(5).is_none());
    assert!(expr.find_node(6).is_none());
    assert!(expr.find_node(7).is_some());
    assert!(expr.find_node(8).is_none());
}

#[test]
fn find_node_tuple() {
    let expr = compile_expression(r#"#(1, 2, 3)"#);
    assert!(expr.find_node(1).is_none());
    assert!(expr.find_node(2).is_some());
    assert!(expr.find_node(3).is_none());
    assert!(expr.find_node(4).is_none());
    assert!(expr.find_node(5).is_some());
    assert!(expr.find_node(6).is_none());
    assert!(expr.find_node(7).is_none());
    assert!(expr.find_node(8).is_some());
    assert!(expr.find_node(9).is_none());
}

#[test]
fn find_node_binop() {
    let expr = compile_expression(r#"1 + 2"#);
    assert!(expr.find_node(0).is_some());
    assert!(expr.find_node(1).is_none());
    assert!(expr.find_node(2).is_none());
    assert!(expr.find_node(3).is_none());
    assert!(expr.find_node(4).is_some());
    assert!(expr.find_node(5).is_none());
}

#[test]
fn find_node_tuple_index() {
    let expr = compile_expression(r#"#(1).0"#);

    let int = TypedExpr::Int {
        location: SrcSpan { start: 2, end: 3 },
        value: "1".into(),
        typ: type_::int(),
    };

    assert_eq!(expr.find_node(0), Some(&expr));
    assert_eq!(expr.find_node(2), Some(&int));
    assert_eq!(expr.find_node(3), Some(&expr));
    assert_eq!(expr.find_node(4), Some(&expr));
    assert_eq!(expr.find_node(5), Some(&expr));
    assert_eq!(expr.find_node(6), None);
}

#[test]
fn find_node_module_select() {
    let expr = TypedExpr::ModuleSelect {
        location: SrcSpan { start: 1, end: 3 },
        typ: type_::int(),
        label: "label".into(),
        module_name: vec!["name".into()],
        module_alias: "alias".into(),
        constructor: ModuleValueConstructor::Fn,
    };

    assert_eq!(expr.find_node(0), None);
    assert_eq!(expr.find_node(1), Some(&expr));
    assert_eq!(expr.find_node(2), Some(&expr));
    assert_eq!(expr.find_node(3), None);
}
