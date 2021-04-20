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

#[test]
fn boolean_constants() {
    assert_js!(
        r#"
const a = True
const b = False
const c = Nil
"#,
        r#""use strict";

const a = true;

const b = false;

const c = null;
"#
    );
}
