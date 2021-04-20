use crate::assert_js;

#[test]
fn boolean_literals() {
    assert_js!(
        r#"
fn go() {
    True
    False
    Nil
}
"#,
        r#""use strict";

function go() {
  true;
  false;
  return null;
}
"#
    );
}
