use std::{cell::RefCell, sync::Arc};

use crate::{
    ast::{SrcSpan, TypedExpr, TypedPattern},
    build::Located,
    type_::{
        self, AccessorsMap, Environment, ExprTyper, FieldMap, ModuleValueConstructor,
        RecordAccessor, Type, TypeVar, ValueConstructor, ValueConstructorVariant,
    },
    uid::UniqueIdGenerator,
};

use super::TypedModule;

fn compile_module(src: &str) -> TypedModule {
    use crate::type_::{build_prelude, infer_module};
    let (ast, _) = crate::parse::parse_module(src).expect("syntax error");
    let ids = UniqueIdGenerator::new();
    let mut modules = im::HashMap::new();
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
    infer_module(
        crate::build::Target::Erlang,
        &ids,
        ast,
        crate::build::Origin::Src,
        "thepackage",
        &modules,
        &mut vec![],
    )
    .expect("should successfully infer")
}

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
        arity: 2,
        location: SrcSpan { start: 12, end: 15 },
        field_map: Some(FieldMap {
            arity: 2,
            fields: [("name".into(), 0), ("age".into(), 1)].into(),
        }),
        module: "mymod".into(),
    };
    environment.insert_variable(
        "Cat".into(),
        variant,
        type_::fn_(vec![type_::string(), type_::int()], cat_type.clone()),
    );

    environment.insert_accessors(
        "Cat",
        AccessorsMap {
            public: true,
            type_: cat_type,
            accessors: [
                (
                    "name".into(),
                    RecordAccessor {
                        index: 0,
                        label: "name".into(),
                        type_: type_::string(),
                    },
                ),
                (
                    "age".into(),
                    RecordAccessor {
                        index: 1,
                        label: "age".into(),
                        type_: type_::int(),
                    },
                ),
            ]
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
    assert_eq!(expr.find_node(1), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(4), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(5), None);
}

#[test]
fn find_node_todo_with_string() {
    let expr = compile_expression(r#" todo("ok") "#);
    assert_eq!(expr.find_node(0), None);
    assert_eq!(expr.find_node(1), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(10), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(11), None);
}

#[test]
fn find_node_string() {
    let expr = compile_expression(r#" "ok" "#);
    assert_eq!(expr.find_node(0), None);
    assert_eq!(expr.find_node(1), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(4), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(5), None);
}

#[test]
fn find_node_float() {
    let expr = compile_expression(r#" 1.02 "#);
    assert_eq!(expr.find_node(0), None);
    assert_eq!(expr.find_node(1), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(4), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(5), None);
}

#[test]
fn find_node_int() {
    let expr = compile_expression(r#" 1302 "#);
    assert_eq!(expr.find_node(0), None);
    assert_eq!(expr.find_node(1), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(4), Some(Located::Expression(expr.clone())));
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
            variant: ValueConstructorVariant::LocalVariable {
                location: SrcSpan { start: 4, end: 10 },
            },
            type_: type_::int(),
        },
        name: "wibble".into(),
    };

    assert_eq!(expr.find_node(14), None);
    assert_eq!(expr.find_node(15), Some(Located::Expression(var.clone())));
    assert_eq!(expr.find_node(20), Some(Located::Expression(var.clone())));
    assert_eq!(expr.find_node(21), None);
}

#[test]
fn find_node_sequence() {
    let sequence = compile_expression(r#"1 2 3"#);
    assert!(sequence.find_node(0).is_some());
    assert!(sequence.find_node(1).is_none());
    assert!(sequence.find_node(2).is_some());
    assert!(sequence.find_node(3).is_none());
    assert!(sequence.find_node(4).is_some());
    assert!(sequence.find_node(5).is_none());
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

    assert_eq!(list.find_node(0), Some(Located::Expression(list.clone())));
    assert_eq!(list.find_node(1), Some(Located::Expression(int1.clone())));
    assert_eq!(list.find_node(2), Some(Located::Expression(list.clone())));
    assert_eq!(list.find_node(3), Some(Located::Expression(list.clone())));
    assert_eq!(list.find_node(4), Some(Located::Expression(int2.clone())));
    assert_eq!(list.find_node(5), Some(Located::Expression(list.clone())));
    assert_eq!(list.find_node(6), Some(Located::Expression(list.clone())));
    assert_eq!(list.find_node(7), Some(Located::Expression(int3.clone())));
    assert_eq!(list.find_node(8), Some(Located::Expression(list.clone())));
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

    assert_eq!(tuple.find_node(0), Some(Located::Expression(tuple.clone())));
    assert_eq!(tuple.find_node(1), Some(Located::Expression(tuple.clone())));
    assert_eq!(tuple.find_node(2), Some(Located::Expression(int1.clone())));
    assert_eq!(tuple.find_node(3), Some(Located::Expression(tuple.clone())));
    assert_eq!(tuple.find_node(4), Some(Located::Expression(tuple.clone())));
    assert_eq!(tuple.find_node(5), Some(Located::Expression(int2.clone())));
    assert_eq!(tuple.find_node(6), Some(Located::Expression(tuple.clone())));
    assert_eq!(tuple.find_node(7), Some(Located::Expression(tuple.clone())));
    assert_eq!(tuple.find_node(8), Some(Located::Expression(int3.clone())));
    assert_eq!(tuple.find_node(9), Some(Located::Expression(tuple.clone())));
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

    assert_eq!(expr.find_node(2), Some(Located::Expression(int.clone())));
    assert_eq!(expr.find_node(4), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(5), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(6), None);
}

#[test]
fn find_node_module_select() {
    let expr = TypedExpr::ModuleSelect {
        location: SrcSpan { start: 1, end: 3 },
        typ: type_::int(),
        label: "label".into(),
        module_name: "name".into(),
        module_alias: "alias".into(),
        constructor: ModuleValueConstructor::Fn {
            location: SrcSpan { start: 1, end: 55 },
        },
    };

    assert_eq!(expr.find_node(0), None);
    assert_eq!(expr.find_node(1), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(2), Some(Located::Expression(expr.clone())));
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

    assert_eq!(expr.find_node(0), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(6), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(7), Some(Located::Expression(int.clone())));
    assert_eq!(expr.find_node(8), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(9), Some(Located::Expression(expr.clone())));
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

    assert_eq!(expr.find_node(11), Some(Located::Expression(retrn)));
    assert_eq!(expr.find_node(14), None);
    assert_eq!(expr.find_node(15), Some(Located::Expression(arg1)));
    assert_eq!(expr.find_node(16), None);
    assert_eq!(expr.find_node(18), Some(Located::Expression(arg2)));
    assert_eq!(expr.find_node(19), None);
}

#[test]
fn find_node_assignment() {
    let expr = compile_expression("let a = 123");

    let val = TypedExpr::Int {
        location: SrcSpan { start: 8, end: 11 },
        value: "123".into(),
        typ: type_::int(),
    };
    let ptn = TypedPattern::Var {
        location: SrcSpan { start: 4, end: 5 },
        name: "a".to_string(),
    };

    let typ = Type::App {
        public: true,
        module: vec![],
        name: "Int".to_string(),
        args: vec![],
    };
    assert_eq!(expr.find_node(0), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(1), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(2), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(3), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(4), Some(Located::Pattern(ptn, typ)));
    assert_eq!(expr.find_node(5), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(5), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(6), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(7), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(8), Some(Located::Expression(val.clone())));
    assert_eq!(expr.find_node(9), Some(Located::Expression(val.clone())));
    assert_eq!(expr.find_node(10), Some(Located::Expression(val.clone())));
}

#[test]
fn find_node_tuple_constructor_assignment() {
    let module = compile_module("pub type D { D(a: Int) }; pub fn abc(r: D) { assert D(a) = r }");
    let ptn = TypedPattern::Var {
        location: SrcSpan { start: 54, end: 55 },
        name: "a".to_string(),
    };

    let typ = Type::App {
        public: true,
        module: vec![],
        name: "Int".to_string(),
        args: vec![],
    };
    assert_eq!(module.find_node(54), Some(Located::Pattern(ptn, typ)));
}

#[test]
fn find_node_tuple_assignment() {
    let expr = compile_expression("let #(a, b) = #(123, 456)");

    let ptn = TypedPattern::Var {
        location: SrcSpan { start: 6, end: 7 },
        name: "a".to_string(),
    };

    let typ = Type::App {
        public: true,
        module: vec![],
        name: "Int".to_string(),
        args: vec![],
    };

    assert_eq!(expr.find_node(0), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(4), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(5), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(5), Some(Located::Expression(expr.clone())));
    assert_eq!(expr.find_node(6), Some(Located::Pattern(ptn, typ)));
}

#[test]
fn find_node_record_access() {
    let access = compile_expression(r#"Cat("Nubi", 3).name"#);

    let string = TypedExpr::String {
        location: SrcSpan { start: 4, end: 10 },
        value: "Nubi".into(),
        typ: type_::string(),
    };

    let int = TypedExpr::Int {
        location: SrcSpan { start: 12, end: 13 },
        value: "3".into(),
        typ: type_::int(),
    };

    assert_eq!(
        access.find_node(4),
        Some(Located::Expression(string.clone()))
    );
    assert_eq!(
        access.find_node(9),
        Some(Located::Expression(string.clone()))
    );
    assert_eq!(access.find_node(12), Some(Located::Expression(int)));
    assert_eq!(
        access.find_node(14),
        Some(Located::Expression(access.clone()))
    );
    assert_eq!(
        access.find_node(18),
        Some(Located::Expression(access.clone()))
    );
    assert_eq!(access.find_node(19), None);
}

#[test]
fn find_node_record_update() {
    let update = compile_expression(r#"Cat(..Cat("Nubi", 3), age: 4)"#);
    let located = Some(Located::Expression(update.clone()));

    let int = TypedExpr::Int {
        location: SrcSpan { start: 27, end: 28 },
        value: "4".into(),
        typ: type_::int(),
    };

    assert_eq!(update.find_node(0), located);
    assert_eq!(update.find_node(3), located);
    assert_eq!(update.find_node(27), Some(Located::Expression(int)));
    assert_eq!(update.find_node(28), located);
    assert_eq!(update.find_node(29), None);
}

#[test]
fn find_node_try() {
    let try_ = compile_expression(r#"try x = Ok(1) Ok(2)"#);

    let int1 = TypedExpr::Int {
        location: SrcSpan { start: 11, end: 12 },
        value: "1".into(),
        typ: type_::int(),
    };

    let int2 = TypedExpr::Int {
        location: SrcSpan { start: 17, end: 18 },
        value: "2".into(),
        typ: type_::int(),
    };

    let ptn = TypedPattern::Var {
        location: SrcSpan { start: 4, end: 5 },
        name: "x".to_string(),
    };

    let typ = Type::Var {
        type_: Arc::new(RefCell::new(TypeVar::Link {
            type_: Arc::new(Type::App {
                public: true,
                module: vec![],
                name: "Int".to_string(),
                args: vec![],
            }),
        })),
    };

    assert_eq!(try_.find_node(0), Some(Located::Expression(try_.clone())));
    assert_eq!(try_.find_node(4), Some(Located::Pattern(ptn, typ)));
    assert_eq!(try_.find_node(11), Some(Located::Expression(int1.clone())));
    assert_eq!(try_.find_node(17), Some(Located::Expression(int2.clone())));
    assert_eq!(try_.find_node(19), None);
}

#[test]
fn find_node_case() {
    let case = compile_expression(
        r#"
case 1, 2 {
  _, _ -> 3
}
"#,
    );

    let int1 = TypedExpr::Int {
        location: SrcSpan { start: 6, end: 7 },
        value: "1".into(),
        typ: type_::int(),
    };

    let int2 = TypedExpr::Int {
        location: SrcSpan { start: 9, end: 10 },
        value: "2".into(),
        typ: type_::int(),
    };

    let int3 = TypedExpr::Int {
        location: SrcSpan { start: 23, end: 24 },
        value: "3".into(),
        typ: type_::int(),
    };

    assert_eq!(case.find_node(1), Some(Located::Expression(case.clone())));
    assert_eq!(case.find_node(6), Some(Located::Expression(int1.clone())));
    assert_eq!(case.find_node(9), Some(Located::Expression(int2.clone())));
    assert_eq!(case.find_node(23), Some(Located::Expression(int3.clone())));
    assert_eq!(case.find_node(25), Some(Located::Expression(case.clone())));
    assert_eq!(case.find_node(26), None);
}

#[test]
fn find_node_bool() {
    let negate = compile_expression(r#"!True"#);

    let bool = TypedExpr::Var {
        location: SrcSpan { start: 1, end: 5 },
        constructor: ValueConstructor {
            public: true,
            variant: ValueConstructorVariant::Record {
                name: "True".into(),
                arity: 0,
                field_map: None,
                location: SrcSpan { start: 0, end: 0 },
                module: "".into(),
            },
            type_: type_::bool(),
        },
        name: "True".into(),
    };

    assert_eq!(
        negate.find_node(0),
        Some(Located::Expression(negate.clone()))
    );
    assert_eq!(negate.find_node(1), Some(Located::Expression(bool.clone())));
    assert_eq!(negate.find_node(2), Some(Located::Expression(bool.clone())));
    assert_eq!(negate.find_node(3), Some(Located::Expression(bool.clone())));
    assert_eq!(negate.find_node(4), Some(Located::Expression(bool.clone())));
    assert_eq!(negate.find_node(5), None);
}

#[test]
fn find_node_import() {
    let module = compile_module(
        r#"

pub fn main() {
  Nil
}

"#,
    );

    assert_eq!(module.find_node(0), Some(Located::OutsideAnyStatement));
    assert_eq!(module.find_node(1), Some(Located::OutsideAnyStatement));
    assert_ne!(module.find_node(2), Some(Located::OutsideAnyStatement));
    assert_ne!(module.find_node(22), Some(Located::OutsideAnyStatement));
    assert_eq!(module.find_node(23), Some(Located::OutsideAnyStatement));
    assert_eq!(module.find_node(24), Some(Located::OutsideAnyStatement));
}

#[test]
fn find_node_try_pattern() {
    let try_ = compile_expression(r#"try #(cccc, dddd) = Ok(#("a", 1)) Ok(Nil)"#);
    let ptn = TypedPattern::Var {
        location: SrcSpan { start: 6, end: 10 },
        name: "cccc".to_string(),
    };

    let typ = Type::App {
        public: true,
        module: vec![],
        name: "String".to_string(),
        args: vec![],
    };
    assert_eq!(try_.find_node(0), Some(Located::Expression(try_.clone())));
    assert_eq!(try_.find_node(6), Some(Located::Pattern(ptn, typ)));

    let ptn = TypedPattern::Var {
        location: SrcSpan { start: 12, end: 16 },
        name: "dddd".to_string(),
    };

    let typ = Type::App {
        public: true,
        module: vec![],
        name: "Int".to_string(),
        args: vec![],
    };
    assert_eq!(try_.find_node(12), Some(Located::Pattern(ptn, typ)));
}

#[test]
fn find_node_try_pattern_discard() {
    let try_ = compile_expression(r#"try #(_ccc, _ddd) = Ok(#("a", 1)) Ok(Nil)"#);
    let ptn = TypedPattern::Discard {
        location: SrcSpan { start: 6, end: 10 },
        name: "_ccc".to_string(),
    };

    let typ = Type::App {
        public: true,
        module: vec![],
        name: "String".to_string(),
        args: vec![],
    };
    assert_eq!(try_.find_node(0), Some(Located::Expression(try_.clone())));
    assert_eq!(try_.find_node(6), Some(Located::Pattern(ptn, typ)));

    let ptn = TypedPattern::Discard {
        location: SrcSpan { start: 12, end: 16 },
        name: "_ddd".to_string(),
    };

    let typ = Type::App {
        public: true,
        module: vec![],
        name: "Int".to_string(),
        args: vec![],
    };
    assert_eq!(try_.find_node(12), Some(Located::Pattern(ptn, typ)));
}
