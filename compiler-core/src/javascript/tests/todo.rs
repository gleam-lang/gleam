use crate::{assert_js, assert_ts_def};

#[test]
fn without_message() {
    assert_js!(
        r#"
fn go() {
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
fn go() {
  todo("I should do this")
};
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1238
#[test]
fn as_expression() {
    assert_js!(
        r#"
fn go(f) {
  let boop = todo("I should do this")
  f(todo("Boom"))
};
"#,
    );
}
