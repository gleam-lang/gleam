use crate::{assert_js, assert_ts_def};

#[test]
fn bare() {
    assert_js!(
        r#"
pub fn go() {
  panic
}
"#,
    );
}

#[test]
fn panic_as() {
    assert_js!(
        r#"
pub fn go() {
  let x = "wibble"
  panic as x
}
"#,
    );
}

#[test]
fn bare_typescript() {
    assert_ts_def!(
        r#"
pub fn go() {
  panic
}
"#,
    );
}

#[test]
fn as_expression() {
    assert_js!(
        r#"
pub fn go(f) {
  let boop = panic
  f(panic)
}
"#,
    );
}

#[test]
fn pipe() {
    assert_js!(
        r#"
pub fn go(f) {
  f |> panic
}
"#,
    );
}

#[test]
fn sequence() {
    assert_js!(
        r#"
pub fn go(at_the_disco) {
  panic
  at_the_disco
}
"#,
    );
}

#[test]
fn case() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    _ -> panic
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/4471
#[test]
fn case_message() {
    assert_js!(
        r#"
pub fn crash(message) {
  panic as case message {
    Ok(message) -> message
    Error(_) -> "No message provided"
  }
}
"#
    );
}
