use crate::assert_js;

// https://github.com/gleam-lang/gleam/issues/1187
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

// https://github.com/gleam-lang/gleam/issues/1188
#[test]
fn following_todo() {
    assert_js!(
        r#"
fn go(x) {
  case x {
    True -> todo
    _ -> 1
  }
}
"#,
        r#""use strict";

function go(x) {
  if (x) {
    throw Object.assign(
      new Error("This has not yet been implemented"),
      { gleam_error: "todo", module: "my/mod", function: "go", line: 4 }
    );
  } else {
    return 1;
  }
}
"#
    )
}
