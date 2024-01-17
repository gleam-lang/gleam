use ecow::EcoString;
use itertools::Itertools;

use crate::type_::expression::Implementations;

use super::compile_module;

macro_rules! assert_targets {
    ($src:expr, $implementations:expr $(,)?) => {
        let result = $crate::type_::tests::expression::implementations($src);
        let expected = $implementations
            .iter()
            .map(|(name, expected_impl)| ((*name).into(), *expected_impl))
            .collect_vec();
        assert_eq!(expected, result);
    };
}

pub fn implementations(src: &str) -> Vec<(EcoString, Implementations)> {
    compile_module(src, None, vec![])
        .expect("compile src")
        .type_info
        .values
        .into_iter()
        .map(|(name, value)| (name, value.variant.implementations()))
        .sorted()
        .collect_vec()
}

#[test]
pub fn pure_gleam_function() {
    assert_targets!(
        r#"
pub fn pure_gleam_1() { 1 + 1 }
pub fn pure_gleam_2() { pure_gleam_1() * 2 }
"#,
        vec![
            ("pure_gleam_1", Implementations::no_externals()),
            ("pure_gleam_2", Implementations::no_externals())
        ],
    );
}

#[test]
pub fn erlang_only_function() {
    assert_targets!(
        r#"
@external(erlang, "foo", "bar")
pub fn erlang_only_1() -> Int

pub fn erlang_only_2() { erlang_only_1() * 2 }
"#,
        vec![
            ("erlang_only_1", Implementations::erlang()),
            ("erlang_only_2", Implementations::erlang()),
        ],
    );
}

#[test]
pub fn externals_only_function() {
    assert_targets!(
        r#"
@external(erlang, "foo", "bar")
@external(javascript, "foo", "bar")
pub fn all_externals_1() -> Int

pub fn all_externals_2() { all_externals_1() * 2 }
"#,
        vec![
            ("all_externals_1", Implementations::all_externals()),
            ("all_externals_2", Implementations::all_externals())
        ],
    );
}

#[test]
pub fn externals_with_pure_gleam_body() {
    assert_targets!(
        r#"
@external(javascript, "foo", "bar")
pub fn javascript_external_and_pure_body() -> Int { 1 + 1 }

@external(erlang, "foo", "bar")
pub fn erlang_external_and_pure_body() -> Int { 1 + 1 }

pub fn pure_gleam() {
  javascript_external_and_pure_body() + erlang_external_and_pure_body()
}
"#,
        vec![
            (
                "erlang_external_and_pure_body",
                Implementations {
                    gleam: true,
                    erlang: true,
                    javascript: false
                }
            ),
            (
                "javascript_external_and_pure_body",
                Implementations {
                    gleam: true,
                    erlang: false,
                    javascript: true
                }
            ),
            (
                "pure_gleam",
                Implementations {
                    gleam: true,
                    erlang: true,
                    javascript: true
                }
            )
        ],
    );
}

#[test]
pub fn erlang_external_with_javascript_body() {
    assert_targets!(
        r#"
@external(javascript, "foo", "bar")
pub fn javascript_only() -> Int

@external(erlang, "foo", "bar")
pub fn erlang_external_and_javascript_body() -> Int { javascript_only() }

pub fn all_externals() -> Int { erlang_external_and_javascript_body() }
"#,
        vec![
            ("all_externals", Implementations::all_externals()),
            (
                "erlang_external_and_javascript_body",
                Implementations::all_externals()
            ),
            ("javascript_only", Implementations::javascript()),
        ],
    );
}

#[test]
pub fn javascript_external_with_erlang_body() {
    assert_targets!(
        r#"
@external(erlang, "foo", "bar")
pub fn erlang_only() -> Int

@external(javascript, "foo", "bar")
pub fn javascript_external_and_erlang_body() -> Int { erlang_only() }

pub fn all_externals() -> Int { javascript_external_and_erlang_body() }
"#,
        vec![
            ("all_externals", Implementations::all_externals()),
            ("erlang_only", Implementations::erlang()),
            (
                "javascript_external_and_erlang_body",
                Implementations::all_externals()
            ),
        ],
    );
}
