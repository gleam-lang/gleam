use crate::assert_js;

#[test]
fn pointless() {
    assert_js!(
        r#"
fn go(x) {
  case x {
    _ -> x
  }
}
"#,
        r#""use strict";

function go(x) {
  return x;
}
"#
    )
}
