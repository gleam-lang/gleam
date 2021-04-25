use crate::assert_js;

#[test]
fn zero_arity_custom_type() {
    assert_js!(
        r#"
type Mine{
    This
    That
}

fn go() {
    This
}
"#,
r#""use strict";

function go() {
  return {type: "This"};
}
"#
    );
}