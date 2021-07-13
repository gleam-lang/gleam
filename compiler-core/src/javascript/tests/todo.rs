use crate::assert_js;

#[test]
fn without_message() {
    assert_js!(
        r#"
fn go() {
    todo
}
"#,
        r#""use strict";

function go() {
  throw Object.assign(
    new Error("This has not yet been implemented"),
    { gleam_error: "todo", module: "my/mod", function: "go", line: 3 }
  );
}
"#
    );
}

#[test]
fn with_message() {
    assert_js!(
        r#"
fn go() {
    todo("I should do this");
};
"#,
        r#""use strict";

function go() {
  throw Object.assign(
    new Error("I should do this"),
    { gleam_error: "todo", module: "my/mod", function: "go", line: 3 }
  );
}
"#
    );
}
