use crate::{assert_js, assert_ts_def};

#[test]
fn tuple_matching() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert #(1, 2) = x
}
"#,
    )
}

#[test]
fn assert() {
    assert_js!(r#"pub fn go(x) { let assert 1 = x }"#,);
}

#[test]
fn assert1() {
    assert_js!(r#"pub fn go(x) { let assert #(1, 2) = x }"#,);
}

#[test]
fn nested_binding() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert #(a, #(b, c, 2) as t, _, 1) = x
}
"#,
    )
}

#[test]
fn variable_renaming() {
    assert_js!(
        r#"

pub fn go(x, wibble) {
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

pub fn go() {
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
    assert_js!(r#"pub fn go(x) { let assert 1 = x + 1 }"#,);
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

// https://github.com/gleam-lang/gleam/issues/3894
#[test]
fn let_assert_nested_string_prefix() {
    assert_js!(
        r#"
type Wibble {
  Wibble(wibble: String)
}

pub fn main() {
  let assert Wibble(wibble: "w" as prefix <> rest) = Wibble("wibble")
  prefix <> rest
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

#[test]
fn message() {
    assert_js!(
        r#"
pub fn unwrap_or_panic(value) {
  let assert Ok(inner) = value as "Oops, there was an error"
  inner
}
"#
    );
}

#[test]
fn variable_message() {
    assert_js!(
        r#"
pub fn expect(value, message) {
  let assert Ok(inner) = value as message
  inner
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/4471
#[test]
fn case_message() {
    assert_js!(
        r#"
pub fn expect(value, message) {
  let assert Ok(inner) = value as case message {
    Ok(message) -> message
    Error(_) -> "No message provided"
  }
  inner
}
"#
    );
}

#[test]
fn assert_that_always_succeeds() {
    assert_js!(
        r#"
type Wibble {
    Wibble(Int)
}

pub fn go() {
  let assert Wibble(n) = Wibble(1)
  n
}
"#,
    );
}

#[test]
fn assert_that_always_fails() {
    assert_js!(
        r#"
type Wibble {
    Wibble(Int)
    Wobble(Int)
}

pub fn go() {
  let assert Wobble(n) = Wibble(1)
  n
}
"#,
    );
}

#[test]
fn catch_all_assert() {
    assert_js!(
        r#"
type Wibble {
    Wibble(Int)
    Wobble(Int)
}

pub fn go() {
  let assert _ = Wibble(1)
  1
}
"#,
    );
}

#[test]
fn assert_with_multiple_variants() {
    assert_js!(
        r#"
type Wibble {
    Wibble(Int)
    Wobble(Int)
    Woo(Int)
}

pub fn go() {
  let assert Wobble(n) = todo
  n
}
"#,
    );
}

#[test]
fn use_discard_assignment() {
    assert_js!(
        r#"
type Wibble {
    Wibble(Int)
    Wobble(Int)
    Woo(Int)
}

fn fun(f) { f(Wibble(1)) }

pub fn go() {
  use _ <- fun
  1
}
"#,
    );
}

#[test]
fn use_matching_assignment() {
    assert_js!(
        r#"
fn fun(f) { f(#(2, 4)) }

pub fn go() {
  use #(_, n) <- fun
  n
}
"#,
    );
}
