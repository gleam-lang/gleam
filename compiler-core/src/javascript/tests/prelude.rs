use crate::assert_js;

#[test]
fn qualified_ok() {
    assert_js!(
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
