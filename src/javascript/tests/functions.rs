use crate::assert_js;


#[test]
fn exported_functions(){
    assert_js!(
        r#"
pub fn add(x, y) {
    x + y
}"#,
r#""use strict";

export function add(x, y) {
  return x + y;
}
"#
    );
}