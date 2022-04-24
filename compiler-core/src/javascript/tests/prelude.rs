use crate::{assert_js, assert_ts_def};

#[test]
fn qualified_ok() {
    assert_js!(
        r#"import gleam
pub fn go() { gleam.Ok(1) }
"#,
    );
}

#[test]
fn qualified_ok_typescript() {
    assert_ts_def!(
        r#"import gleam
pub fn go() { gleam.Ok(1) }
"#,
    );
}

#[test]
fn qualified_error() {
    assert_js!(
        r#"import gleam
pub fn go() { gleam.Error(1) }
"#,
    );
}

#[test]
fn qualified_nil() {
    assert_js!(
        r#"import gleam
pub fn go() { gleam.Nil }
"#,
    );
}

#[test]
fn qualified_nil_typescript() {
    assert_ts_def!(
        r#"import gleam
pub fn go() { gleam.Nil }
"#,
    );
}
