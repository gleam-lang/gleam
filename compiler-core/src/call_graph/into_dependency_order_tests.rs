use super::*;
use crate::{
    ast::{Arg, Function, ModuleConstant, Publicity},
    type_::{
        Deprecation,
        expression::{Implementations, Purity},
    },
};
use ecow::EcoString;

type FuncInput = (&'static str, &'static [&'static str], &'static str);
type ConstInput = (&'static str, &'static str);

fn parse_and_order(
    functions: &[FuncInput],
    constants: &[ConstInput],
) -> Result<Vec<Vec<EcoString>>, Error> {
    let functions = functions
        .iter()
        .map(|(name, arguments, src)| Function {
            name: Some((SrcSpan::default(), EcoString::from(*name))),
            arguments: arguments
                .iter()
                .map(|name| Arg {
                    names: crate::ast::ArgNames::Named {
                        name: EcoString::from(*name),
                        location: Default::default(),
                    },
                    location: Default::default(),
                    annotation: None,
                    type_: (),
                })
                .collect_vec(),
            body: crate::parse::parse_statement_sequence(src)
                .expect("syntax error")
                .to_vec(),
            location: Default::default(),
            body_start: None,
            return_annotation: None,
            publicity: Publicity::Public,
            deprecation: Deprecation::NotDeprecated,
            end_position: src.len() as u32,
            return_type: (),
            documentation: None,
            external_erlang: None,
            external_javascript: None,
            implementations: Implementations {
                gleam: true,
                uses_erlang_externals: true,
                uses_javascript_externals: false,
                can_run_on_erlang: true,
                can_run_on_javascript: true,
            },
            purity: Purity::Impure,
        })
        .collect_vec();
    let constants = constants
        .iter()
        .map(|(name, value)| {
            let const_value = crate::parse::parse_const_value(value).expect("syntax error");
            ModuleConstant {
                documentation: None,
                location: Default::default(),
                publicity: Publicity::Public,
                name: EcoString::from(*name),
                name_location: SrcSpan::default(),
                annotation: None,
                value: Box::from(const_value),
                implementations: Implementations {
                    gleam: true,
                    uses_erlang_externals: true,
                    uses_javascript_externals: false,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                },
                type_: (),
                deprecation: Deprecation::NotDeprecated,
            }
        })
        .collect_vec();

    Ok(into_dependency_order(functions, constants)?
        .into_iter()
        .map(|level| {
            level
                .into_iter()
                .map(|function| match function {
                    CallGraphNode::Function(f) => f.name.map(|(_, name)| name).unwrap(),
                    CallGraphNode::ModuleConstant(c) => c.name,
                })
                .collect_vec()
        })
        .collect())
}

#[test]
fn empty() {
    let functions = [];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        Vec::<Vec<EcoString>>::new()
    );
}

#[test]
fn no_deps() {
    let functions = [
        ("a", [].as_slice(), "1"),
        ("b", [].as_slice(), r#""ok""#),
        ("c", [].as_slice(), r#"1"#),
        ("d", [].as_slice(), r#"1.0"#),
        ("e", [].as_slice(), r#"todo"#),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["a"], vec!["b"], vec!["c"], vec!["d"], vec!["e"]]
    );
}

#[test]
fn one_dep() {
    let functions = [
        ("a", [].as_slice(), "1"),
        ("b", [].as_slice(), r#"c"#),
        ("c", [].as_slice(), r#"0"#),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["a"], vec!["c"], vec!["b"]]
    );
}

#[test]
fn unknown_vars() {
    let functions = [
        ("a", [].as_slice(), "1"),
        ("b", [].as_slice(), r#"Nil"#),
        ("c", [].as_slice(), r#"Ok"#),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["a"], vec!["b"], vec!["c"]]
    );
}

#[test]
fn calling_function() {
    let functions = [
        ("a", [].as_slice(), r#"b()"#),
        ("b", [].as_slice(), r#"c(1, 2)"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["c"], vec!["b"], vec!["a"]]
    );
}

#[test]
fn ref_in_call_argument() {
    let functions = [
        ("a", [].as_slice(), r#"c(1, b())"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn sequence() {
    let functions = [
        ("a", [].as_slice(), r#"c({ 1 2 b })"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn tuple() {
    let functions = [
        ("a", [].as_slice(), r#"#(b, c, 1)"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn pipeline() {
    let functions = [
        ("a", [].as_slice(), r#"1 |> b |> c"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn list() {
    let functions = [
        ("a", [].as_slice(), r#"[b, b, c, 1]"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn list_spread() {
    let functions = [
        ("a", [].as_slice(), r#"[b, b, ..c]"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn record_access() {
    let functions = [
        ("a", [].as_slice(), "1"),
        ("b", [].as_slice(), r#"b().wibble"#),
        ("c", [].as_slice(), r#"123"#),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["a"], vec!["b"], vec!["c"]]
    );
}

#[test]
fn binop() {
    let functions = [
        ("a", [].as_slice(), r#"1 + a() + 2 / b() * 4"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn bit_arrays() {
    let functions = [
        ("a", [].as_slice(), r#"<<b, c>>"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn tuple_index() {
    let functions = [
        ("a", [].as_slice(), r#"b.0"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn record_update() {
    let functions = [
        ("a", [].as_slice(), r#"Wibble(..b, wobble: c())"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn negate() {
    let functions = [
        ("a", [].as_slice(), r#"!c()"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn use_() {
    let functions = [
        ("a", [].as_slice(), r#"use x <- c"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn use_shadowing() {
    let functions = [
        ("a", [].as_slice(), r#"123"#),
        ("b", [].as_slice(), r#"{ use c <- a c }"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["a"], vec!["b"], vec!["c"]]
    );
}

#[test]
fn fn_argument_shadowing() {
    let functions = &[
        ("a", [].as_slice(), r#"fn(b) { c b }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn fn_argument_shadowing_then_not() {
    let functions = [
        ("a", [].as_slice(), r#"{ fn(b) { c b } b }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn let_var() {
    let functions = [
        ("a", [].as_slice(), r#"{ let c = b c }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn pattern_int() {
    let functions = [("a", [].as_slice(), r#"{ let 1 = x }"#)];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["a"]]
    );
}

#[test]
fn pattern_float() {
    let functions = [("a", [].as_slice(), r#"{ let 1.0 = x }"#)];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["a"]]
    );
}

#[test]
fn pattern_string() {
    let functions = [("a", [].as_slice(), r#"{ let "1.0" = x }"#)];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["a"]]
    );
}

#[test]
fn pattern_underscore() {
    let functions = [("a", [].as_slice(), r#"{ let _ = x }"#)];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["a"]]
    );
}

#[test]
fn pattern_concat() {
    let functions = [
        ("a", [].as_slice(), r#"{ let "a" <> c = b c }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn pattern_tuple() {
    let functions = [
        ("a", [].as_slice(), r#"{ let #(a, c) = b a c }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn pattern_list() {
    let functions = [
        ("a", [].as_slice(), r#"{ let [a, c] = b a c }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn pattern_list_spread() {
    let functions = [
        ("a", [].as_slice(), r#"{ let [a, ..c] = b a c }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn pattern_bit_array_segment_size_var_usage() {
    let functions = [
        (
            "a",
            [].as_slice(),
            r#"{ let <<y:size(b), _:unit(3)>> = c y }"#,
        ),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn pattern_assign() {
    let functions = [
        ("a", [].as_slice(), r#"{ let 1 as b = c b }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn pattern_constructor() {
    let functions = [
        ("a", [].as_slice(), r#"{ let Ok(b) = c b }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn scope_reset() {
    let functions = [
        ("a", [].as_slice(), r#"{ let x = { let b = 1 b } b }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn case_subject() {
    let functions = [
        ("a", [].as_slice(), r#"case b { _ -> 1 }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn case_subjects() {
    let functions = [
        ("a", [].as_slice(), r#"case b, c { _, _ -> 1 }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["c"], vec!["a"]]
    );
}

#[test]
fn case_pattern_shadow() {
    let functions = [
        ("a", [].as_slice(), r#"case 1 { b -> b }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["a"], vec!["b"], vec!["c"]]
    );
}

#[test]
fn case_use_in_clause() {
    let functions = [
        ("a", [].as_slice(), r#"case 1 { _ -> b }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn case_clause_doesnt_shadow_later_clauses() {
    let functions = [
        ("a", [].as_slice(), r#"case 1 { b -> 1 _ -> b }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn case_clause_doesnt_shadow_after() {
    let functions = [
        ("a", [].as_slice(), r#"{ case 1 { b -> 1 } b }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn guard() {
    let functions = [
        ("a", [].as_slice(), r#"case 1 { _ if b -> 1 }"#),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn big_guard() {
    let functions = [
        (
            "a",
            [].as_slice(),
            r#"case 1 { _ if 1 == 2 || x != #(Ok(b), 123) -> 1 }"#,
        ),
        ("b", [].as_slice(), r#"123"#),
        ("c", [].as_slice(), "1"),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b"], vec!["a"], vec!["c"]]
    );
}

#[test]
fn duplicate_external_function_name() {
    let functions = [("c", [].as_slice(), "1"), ("c", [].as_slice(), "1")];
    _ = parse_and_order(functions.as_slice(), [].as_slice()).unwrap_err();
}

#[test]
fn duplicate_function_name() {
    let functions = [
        ("b", [].as_slice(), r#"123456"#),
        ("b", [].as_slice(), r#"123456"#),
    ];
    _ = parse_and_order(functions.as_slice(), [].as_slice()).unwrap_err();
}

#[test]
fn more_complex_cycle() {
    let functions = [
        ("a1", [].as_slice(), r#"{ a2 }"#),
        ("a2", [].as_slice(), r#"{ a3 a1 }"#),
        ("a3", [].as_slice(), r#"{ a1 }"#),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["a2", "a3", "a1"]]
    );
}

#[test]
fn function_argument_shadowing() {
    let functions = [
        ("a", ["b"].as_slice(), r#"b"#),
        ("b", [].as_slice(), r#"Nil"#),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["a"], vec!["b"]]
    );
}

#[test]
fn constants_and_functions() {
    let functions = [
        ("a", ["b"].as_slice(), r#"b"#),
        ("b", [].as_slice(), r#"c"#),
    ];
    let constants = [("d", r#"c"#), ("c", r#"a"#)];
    assert_eq!(
        parse_and_order(functions.as_slice(), constants.as_slice()).unwrap(),
        vec![vec!["a"], vec!["c"], vec!["b"], vec!["d"]]
    );
}

// https://github.com/gleam-lang/gleam/issues/2275
#[test]
fn bug_2275() {
    let functions = [
        ("one", [].as_slice(), r#"two one"#),
        ("two", [].as_slice(), r#"two"#),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["two"], vec!["one"]]
    );
}

#[test]
fn let_assert_message() {
    let functions = [
        ("a", [].as_slice(), r#"{ let assert True = False as b() }"#),
        ("b", [].as_slice(), r#"a()"#),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b", "a"]]
    );
}

#[test]
fn assert_subject() {
    let functions = [
        ("a", [].as_slice(), r#"{ assert b() }"#),
        ("b", [].as_slice(), r#"a()"#),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b", "a"]]
    );
}

#[test]
fn assert_message() {
    let functions = [
        ("a", [].as_slice(), r#"{ assert False as b() }"#),
        ("b", [].as_slice(), r#"a()"#),
    ];
    assert_eq!(
        parse_and_order(functions.as_slice(), [].as_slice()).unwrap(),
        vec![vec!["b", "a"]]
    );
}
