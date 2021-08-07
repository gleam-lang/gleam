use crate::assert_js;

#[test]
fn qualified_ok() {
    assert_js!(
        r#"import gleam
pub fn go() { gleam.Ok(1) }
"#,
        r#"export function go() {
  return { type: "Ok", 0: 1 };
}
"#
    );
}

#[test]
fn qualified_error() {
    assert_js!(
        r#"import gleam
pub fn go() { gleam.Error(1) }
"#,
        r#"export function go() {
  return { type: "Error", 0: 1 };
}
"#
    );
}

#[test]
fn qualified_nil() {
    assert_js!(
        r#"import gleam
pub fn go() { gleam.Nil }
"#,
        r#"export function go() {
  return undefined;
}
"#
    );
}
