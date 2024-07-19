use crate::{assert_js, assert_ts_def};

#[test]
fn tuple_matching() {
    assert_js!(
        r#"
fn go(x) {
  let assert #(1, 2) = x
}
"#,
    )
}

#[test]
fn assert() {
    assert_js!(r#"fn go(x) { let assert 1 = x }"#,);
}

#[test]
fn assert1() {
    assert_js!(r#"fn go(x) { let assert #(1, 2) = x }"#,);
}

#[test]
fn nested_binding() {
    assert_js!(
        r#"
fn go(x) {
  let assert #(a, #(b, c, 2) as t, _, 1) = x
}
"#,
    )
}

#[test]
fn variable_renaming() {
    assert_js!(
        r#"

fn go(x, wibble) {
  let a = 1
  wibble(a)
  let a = 2
  wibble(a)
  let assert #(a, 3) = x
  let b = a
  wibble(b)
  let c = {
    let a = a
    #(a, b)
  }
  wibble(a)
  // make sure arguments are counted in initial state
  let x = c
  x
}
"#,
    )
}

#[test]
fn constant_assignments() {
    assert_js!(
        r#"
const a = True

fn go() {
  a
  let a = 10
  a + 20
}

fn second() {
  let a = 10
  a + 20
}
"#,
    );
}

#[test]
fn returning_literal_subject() {
    assert_js!(r#"fn go(x) { let assert 1 = x + 1 }"#,);
}

#[test]
fn rebound_argument() {
    assert_js!(
        r#"pub fn main(x) {
  let x = False
  x
}
"#,
    );
}

#[test]
fn rebound_function() {
    assert_js!(
        r#"pub fn x() {
  Nil
}

pub fn main() {
  let x = False
  x
}
"#,
    );
}

#[test]
fn rebound_function_and_arg() {
    assert_js!(
        r#"pub fn x() {
  Nil
}

pub fn main(x) {
  let x = False
  x
}
"#,
    );
}

#[test]
fn variable_used_in_pattern_and_assignment() {
    assert_js!(
        r#"pub fn main(x) {
  let #(x) = #(x)
  x
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1253
#[test]
fn correct_variable_renaming_in_assigned_functions() {
    assert_js!(
        r#"
pub fn debug(x) {
  let x = x
  fn(x) { x + 1 }
}
"#,
    );
}

#[test]
fn module_const_var() {
    assert_js!(
        r#"
pub const int = 42
pub const int_alias = int
pub fn use_int_alias() { int_alias }

pub const compound: #(Int, Int) = #(int, int_alias)
pub fn use_compound() { compound.0 + compound.1 }
"#
    );
}

#[test]
fn module_const_var1() {
    assert_ts_def!(
        r#"
pub const int = 42
pub const int_alias = int
pub const compound: #(Int, Int) = #(int, int_alias)
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2443
#[test]
fn let_assert_string_prefix() {
    assert_js!(
        r#"
pub fn main() {
  let assert "Game " <> id = "Game 1"
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2931
#[test]
fn keyword_assignment() {
    assert_js!(
        r#"
pub fn main() {
  let class = 10
  let debugger = 50
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3004
#[test]
fn escaped_variables_in_constants() {
    assert_js!(
        r#"
pub const class = 5
pub const something = class
"#
    );
}
