use std::sync::Arc;

use crate::{
    ast::{SrcSpan, TypedExpr},
    type_::{
        self, AccessorsMap, Environment, ExprTyper, FieldMap, ModuleValueConstructor,
        RecordAccessor, Type, ValueConstructor, ValueConstructorVariant,
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
    let mut warnings = vec![];
    let mut environment = Environment::new(ids, &[], &modules, &mut warnings);

    // Insert a cat record to use in the tests
    let cat_type = Arc::new(Type::App {
        public: true,
        module: vec![],
        name: "Cat".into(),
        args: vec![],
    });
    let variant = ValueConstructorVariant::Record {
        name: "Cat".into(),
        arity: 1,
        field_map: Some(FieldMap {
            arity: 1,
            fields: [("name".into(), 0)].into(),
        }),
    };
    environment.insert_variable(
        "Cat".into(),
        variant.clone(),
        type_::fn_(vec![type_::string()], cat_type.clone()),
        SrcSpan::default(),
    );

    environment.insert_accessors(
        "Cat",
        AccessorsMap {
            public: true,
            type_: cat_type,
            accessors: [(
                "name".into(),
                RecordAccessor {
                    index: 0,
                    label: "name".into(),
                    type_: type_::string(),
                },
            )]
            .into(),
        },
    );
    ExprTyper::new(&mut environment)
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
    let list = compile_expression(r#"[1, 2, 3]"#);

    let int1 = TypedExpr::Int {
        location: SrcSpan { start: 1, end: 2 },
        typ: type_::int(),
        value: "1".into(),
    };
    let int2 = TypedExpr::Int {
        location: SrcSpan { start: 4, end: 5 },
        typ: type_::int(),
        value: "2".into(),
    };
    let int3 = TypedExpr::Int {
        location: SrcSpan { start: 7, end: 8 },
        typ: type_::int(),
        value: "3".into(),
    };

    assert_eq!(list.find_node(0), Some(&list));
    assert_eq!(list.find_node(1), Some(&int1));
    assert_eq!(list.find_node(2), Some(&list));
    assert_eq!(list.find_node(3), Some(&list));
    assert_eq!(list.find_node(4), Some(&int2));
    assert_eq!(list.find_node(5), Some(&list));
    assert_eq!(list.find_node(6), Some(&list));
    assert_eq!(list.find_node(7), Some(&int3));
    assert_eq!(list.find_node(8), Some(&list));
    assert_eq!(list.find_node(9), None);
}

#[test]
fn find_node_tuple() {
    let tuple = compile_expression(r#"#(1, 2, 3)"#);

    let int1 = TypedExpr::Int {
        location: SrcSpan { start: 2, end: 3 },
        typ: type_::int(),
        value: "1".into(),
    };
    let int2 = TypedExpr::Int {
        location: SrcSpan { start: 5, end: 6 },
        typ: type_::int(),
        value: "2".into(),
    };
    let int3 = TypedExpr::Int {
        location: SrcSpan { start: 8, end: 9 },
        typ: type_::int(),
        value: "3".into(),
    };

    assert_eq!(tuple.find_node(0), Some(&tuple));
    assert_eq!(tuple.find_node(1), Some(&tuple));
    assert_eq!(tuple.find_node(2), Some(&int1));
    assert_eq!(tuple.find_node(3), Some(&tuple));
    assert_eq!(tuple.find_node(4), Some(&tuple));
    assert_eq!(tuple.find_node(5), Some(&int2));
    assert_eq!(tuple.find_node(6), Some(&tuple));
    assert_eq!(tuple.find_node(7), Some(&tuple));
    assert_eq!(tuple.find_node(8), Some(&int3));
    assert_eq!(tuple.find_node(9), Some(&tuple));
    assert_eq!(tuple.find_node(10), None);
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

    assert_eq!(expr.find_node(2), Some(&int));
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

#[test]
fn find_node_fn() {
    let expr = compile_expression("fn() { 1 }");

    let int = TypedExpr::Int {
        location: SrcSpan { start: 7, end: 8 },
        value: "1".into(),
        typ: type_::int(),
    };

    assert_eq!(expr.find_node(0), Some(&expr));
    assert_eq!(expr.find_node(6), Some(&expr));
    assert_eq!(expr.find_node(7), Some(&int));
    assert_eq!(expr.find_node(8), Some(&expr));
    assert_eq!(expr.find_node(9), Some(&expr));
    assert_eq!(expr.find_node(10), None);
}

#[test]
fn find_node_call() {
    let expr = compile_expression("fn(_, _) { 1 }(1, 2)");

    let retrn = TypedExpr::Int {
        location: SrcSpan { start: 11, end: 12 },
        value: "1".into(),
        typ: type_::int(),
    };

    let arg1 = TypedExpr::Int {
        location: SrcSpan { start: 15, end: 16 },
        value: "1".into(),
        typ: type_::int(),
    };

    let arg2 = TypedExpr::Int {
        location: SrcSpan { start: 18, end: 19 },
        value: "2".into(),
        typ: type_::int(),
    };

    assert_eq!(expr.find_node(11), Some(&retrn));
    assert_eq!(expr.find_node(14), Some(&expr));
    assert_eq!(expr.find_node(15), Some(&arg1));
    assert_eq!(expr.find_node(16), Some(&expr));
    assert_eq!(expr.find_node(18), Some(&arg2));
    assert_eq!(expr.find_node(19), Some(&expr));
}

#[test]
fn find_node_record_access() {
    let access = compile_expression(r#"Cat("Nubi").name"#);

    let string = TypedExpr::String {
        location: SrcSpan { start: 4, end: 10 },
        value: "Nubi".into(),
        typ: type_::string(),
    };

    assert_eq!(access.find_node(4), Some(&string));
    assert_eq!(access.find_node(9), Some(&string));
    assert_eq!(access.find_node(11), Some(&access));
    assert_eq!(access.find_node(14), Some(&access));
    assert_eq!(access.find_node(15), Some(&access));
    assert_eq!(access.find_node(16), None);
}
