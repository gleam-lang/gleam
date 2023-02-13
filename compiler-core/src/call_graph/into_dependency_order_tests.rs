use super::*;
use crate::ast::{ModuleFunction, TypeAst};
use smol_str::SmolStr;

enum Input {
    Internal(&'static str, &'static str),
    External(&'static str),
}

fn parse_and_order(functions: &[Input]) -> Result<Vec<Vec<SmolStr>>, Error> {
    let functions = functions
        .iter()
        .map(|input| match input {
            Input::Internal(name, src) => ModuleFunction::Internal(Function {
                name: name.into(),
                arguments: vec![],
                body: crate::parse::parse_expression_sequence(src).expect("syntax error"),
                location: Default::default(),
                return_annotation: None,
                public: true,
                end_position: src.len() as u32,
                return_type: (),
                doc: None,
            }),
            Input::External(name) => ModuleFunction::External(ExternalFunction {
                name: name.into(),
                arguments: vec![],
                module: "themodule".into(),
                fun: name.into(),
                location: Default::default(),
                public: true,
                return_: TypeAst::Hole {
                    location: Default::default(),
                    name: "_".into(),
                },
                return_type: (),
                doc: None,
            }),
        })
        .collect_vec();
    Ok(into_dependency_order(functions)?
        .into_iter()
        .map(|level| {
            level
                .into_iter()
                .map(|function| function.name().clone())
                .collect_vec()
        })
        .collect())
}

#[test]
fn empty() {
    let functions = [];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        Vec::<Vec<SmolStr>>::new()
    );
}

#[test]
fn no_deps() {
    let functions = [
        Input::External("a"),
        Input::Internal("b", r#""ok""#),
        Input::Internal("c", r#"1"#),
        Input::Internal("d", r#"1.0"#),
        Input::Internal("e", r#"todo"#),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["a"], vec!["b"], vec!["c"], vec!["d"], vec!["e"]]
    );
}

#[test]
fn one_dep() {
    let functions = [
        Input::External("a"),
        Input::Internal("b", r#"c"#),
        Input::Internal("c", r#"0"#),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["a"], vec!["c"], vec!["b"]]
    );
}

#[test]
fn unknown_vars() {
    let functions = [
        Input::External("a"),
        Input::Internal("b", r#"Nil"#),
        Input::Internal("c", r#"Ok"#),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["a"], vec!["b"], vec!["c"]]
    );
}

#[test]
fn calling_function() {
    let functions = [
        Input::Internal("a", r#"b()"#),
        Input::Internal("b", r#"c(1, 2)"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["c"], vec!["b"], vec!["a"]]
    );
}

#[test]
fn ref_in_call_argument() {
    let functions = [
        Input::Internal("a", r#"c(1, b())"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn sequence() {
    let functions = [
        Input::Internal("a", r#"c({ 1 2 b })"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn tuple() {
    let functions = [
        Input::Internal("a", r#"#(b, c, 1)"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn pipeline() {
    let functions = [
        Input::Internal("a", r#"1 |> b |> c"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn list() {
    let functions = [
        Input::Internal("a", r#"[b, b, c, 1]"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn list_spread() {
    let functions = [
        Input::Internal("a", r#"[b, b, ..c]"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn record_access() {
    let functions = [
        Input::External("a"),
        Input::Internal("b", r#"b().wibble"#),
        Input::Internal("c", r#"123"#),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["a"], vec!["c"], vec!["b"]]
    );
}

#[test]
fn binop() {
    let functions = [
        Input::Internal("a", r#"1 + a() + 2 / b() * 4"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn bit_strings() {
    let functions = [
        Input::Internal("a", r#"<<b, c>>"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn tuple_index() {
    let functions = [
        Input::Internal("a", r#"b.0"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn record_update() {
    let functions = [
        Input::Internal("a", r#"Wibble(..b, wobble: c())"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn negate() {
    let functions = [
        Input::Internal("a", r#"!c()"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn use_() {
    let functions = [
        Input::Internal("a", r#"use x <- c"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn use_shadowing() {
    let functions = [
        Input::Internal("a", r#"123"#),
        Input::Internal("b", r#"{ use c <- a c }"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["a"], vec!["b"], vec!["c"]]
    );
}

#[test]
fn fn_argument_shadowing() {
    let functions = [
        Input::Internal("a", r#"fn(b) { c b }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn fn_argument_shadowing_then_not() {
    let functions = [
        Input::Internal("a", r#"{ fn(b) { c b } b }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn let_var() {
    let functions = [
        Input::Internal("a", r#"{ let c = b c }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn pattern_int() {
    let functions = [Input::Internal("a", r#"{ let 1 = x }"#)];
    assert_eq!(parse_and_order(&functions).unwrap(), vec![vec!["a"]]);
}

#[test]
fn pattern_float() {
    let functions = [Input::Internal("a", r#"{ let 1.0 = x }"#)];
    assert_eq!(parse_and_order(&functions).unwrap(), vec![vec!["a"]]);
}

#[test]
fn pattern_string() {
    let functions = [Input::Internal("a", r#"{ let "1.0" = x }"#)];
    assert_eq!(parse_and_order(&functions).unwrap(), vec![vec!["a"]]);
}

#[test]
fn pattern_underscore() {
    let functions = [Input::Internal("a", r#"{ let _ = x }"#)];
    assert_eq!(parse_and_order(&functions).unwrap(), vec![vec!["a"]]);
}

#[test]
fn pattern_concat() {
    let functions = [
        Input::Internal("a", r#"{ let "a" <> c = b c }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn pattern_tuple() {
    let functions = [
        Input::Internal("a", r#"{ let #(a, c) = b a c }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn pattern_list() {
    let functions = [
        Input::Internal("a", r#"{ let [a, c] = b a c }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn pattern_list_spread() {
    let functions = [
        Input::Internal("a", r#"{ let [a, ..c] = b a c }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn pattern_bit_string_segment_size_var_usage() {
    let functions = [
        Input::Internal("a", r#"{ let <<y:size(b), _:unit(3)>> = c y }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn pattern_assign() {
    let functions = [
        Input::Internal("a", r#"{ let 1 as b = c b }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn pattern_constructor() {
    let functions = [
        Input::Internal("a", r#"{ let Ok(b) = c b }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn scope_reset() {
    let functions = [
        Input::Internal("a", r#"{ let x = { let b = 1 b } b }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn try_() {
    let functions = [
        Input::Internal("a", r#"{ try b = c b }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn case_subject() {
    let functions = [
        Input::Internal("a", r#"case b { _ -> 1 }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn case_subjects() {
    let functions = [
        Input::Internal("a", r#"case b, c { _, _ -> 1 }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn case_pattern_shadow() {
    let functions = [
        Input::Internal("a", r#"case 1 { b -> b }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["a"], vec!["b"], vec!["c"]]
    );
}

#[test]
fn case_use_in_clause() {
    let functions = [
        Input::Internal("a", r#"case 1 { _ -> b }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn case_clause_doesnt_shadow_later_clauses() {
    let functions = [
        Input::Internal("a", r#"case 1 { b -> 1 _ -> b }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn case_clause_doesnt_shadow_after() {
    let functions = [
        Input::Internal("a", r#"{ case 1 { b -> 1 } b }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn guard() {
    let functions = [
        Input::Internal("a", r#"case 1 { _ if b -> 1 }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn big_guard() {
    let functions = [
        Input::Internal("a", r#"case 1 { _ if 1 == 2 || x != #(Ok(b), 123) -> 1 }"#),
        Input::Internal("b", r#"123"#),
        Input::External("c"),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn duplicate_external_function_name() {
    let functions = [Input::External("c"), Input::External("c")];
    _ = parse_and_order(&functions).unwrap_err();
}

#[test]
fn duplicate_function_name() {
    let functions = [
        Input::Internal("b", r#"123456"#),
        Input::Internal("b", r#"123456"#),
    ];
    _ = parse_and_order(&functions).unwrap_err();
}

#[test]
fn more_complex_cycle() {
    let functions = [
        Input::Internal("a1", r#"{ a2 }"#),
        Input::Internal("a2", r#"{ a3 a1 }"#),
        Input::Internal("a3", r#"{ a1 }"#),
    ];
    assert_eq!(
        parse_and_order(&functions).unwrap(),
        vec![vec!["a2", "a1"], vec!["a3"]]
    );
}
