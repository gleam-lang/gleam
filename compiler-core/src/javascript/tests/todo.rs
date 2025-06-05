use crate::{assert_js, assert_ts_def};

#[test]
fn without_message() {
    assert_js!(
        r#"
pub fn go() {
    todo
}
"#,
    );
}

#[test]
fn without_message_typescript() {
    assert_ts_def!(
        r#"
pub fn go() {
    todo
}
"#,
    );
}

#[test]
fn with_message() {
    assert_js!(
        r#"
pub fn go() {
  todo as "I should do this"
}
"#,
    );
}

#[test]
fn with_message_expr() {
    assert_js!(
        r#"
pub fn go() {
  let x = "I should " <> "do this"
  todo as x
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1238
#[test]
fn as_expression() {
    assert_js!(
        r#"
pub fn go(f) {
  let boop = todo as "I should do this"
  f(todo as "Boom")
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/4471
#[test]
fn case_message() {
    assert_js!(
        r#"
pub fn unimplemented(message) {
  panic as case message {
    Ok(message) -> message
    Error(_) -> "No message provided"
  }
}
"#
    );
}

#[test]
fn inside_fn() {
    assert_js!(
        r#"
pub fn main() {
  fn() { todo }
}
"#
    );
}
