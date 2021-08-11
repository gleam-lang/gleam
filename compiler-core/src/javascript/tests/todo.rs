use crate::assert_js;

#[test]
fn without_message() {
    assert_js!(
        r#"
fn go() {
    todo
}
"#,
        r#"function go() {
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
  todo("I should do this")
};
"#,
        r#"function go() {
  throw Object.assign(
    new Error("I should do this"),
    { gleam_error: "todo", module: "my/mod", function: "go", line: 3 }
  );
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1238
#[test]
fn as_expression() {
    assert_js!(
        r#"
fn go(f) {
  let boop = todo("I should do this")
  f(todo("Boom"))
};
"#,
        r#"function go(f) {
  let boop = (() => {
    throw Object.assign(
      new Error("I should do this"),
      { gleam_error: "todo", module: "my/mod", function: "go", line: 3 }
    );
  })();
  return f(
    (() => {
      throw Object.assign(
        new Error("Boom"),
        { gleam_error: "todo", module: "my/mod", function: "go", line: 4 }
      );
    })(),
  );
}
"#
    );
}
