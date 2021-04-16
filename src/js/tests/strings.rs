use super::super::*;
use crate::assert_js;

#[test]
fn string_literals() {
    assert_js!(
        r#"
fn go() {
    "Hello, Gleam!"
}
"#,
        r#"function go() {
  return "Hello, Gleam!";
}"#
    );
}
