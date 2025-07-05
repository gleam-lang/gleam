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

// https://github.com/gleam-lang/gleam/issues/4756
#[test]
fn qualified_prelude_value_does_not_conflict_with_local_value() {
    assert_js!(
        "
import gleam

pub type Result(a, e) {
  Ok(a)
  Error(e)
}

pub fn main() {
  gleam.Ok(10)
}
",
    );
}

#[test]
fn qualified_prelude_value_does_not_conflict_with_local_value_constant() {
    assert_js!(
        r#"
import gleam

pub type Result(a, e) {
  Ok(a)
  Error(e)
}

pub const error = gleam.Error("Bad")
"#,
    );
}

#[test]
fn qualified_prelude_value_does_not_conflict_with_local_value_pattern() {
    assert_js!(
        r#"
import gleam

pub type Result(a, e) {
  Ok(a)
  Error(e)
}

pub fn go(x) {
  case x {
    gleam.Ok(x) -> x
    gleam.Error(_) -> 0
  }
}
"#,
    );
}
