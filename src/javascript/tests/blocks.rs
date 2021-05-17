use crate::assert_js;

#[test]
fn block() {
    assert_js!(
        r#"
fn go() {
  let x = {
    1
    2
  }
  x
}
"#,
        r#""use strict";

function go() {
  let x = (() => {
    1;
    return 2;
  })();
  return x;
}
"#
    );
}

#[test]
fn sequences() {
    assert_js!(
        r#"
fn go() {
  "one"
  "two"
  "three"
}
"#,
        r#""use strict";

function go() {
  "one";
  "two";
  return "three";
}
"#
    );
}
