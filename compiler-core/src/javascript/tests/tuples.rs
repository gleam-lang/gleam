use crate::assert_js;

#[test]
fn tuple() {
    assert_js!(
        r#"
fn go() {
  #("1", "2", "3")
}
"#,
    );

    assert_js!(
        r#"
fn go() {
  #(
    "1111111111111111111111111111111",
    #("1111111111111111111111111111111", "2", "3"),
    "3",
  )
}
"#,
    );
}

#[test]
fn tuple_access() {
    assert_js!(
        r#"
fn go() {
  #(1, 2).0
}
"#,
    )
}

#[test]
fn tuple_with_block_element() {
    assert_js!(
        r#"
fn go() {
  #(
    "1", 
    {
      "2"
      "3"
    },
  )
}
"#,
    );

    assert_js!(
        r#"
fn go() {
  #(
    "1111111111111111111111111111111",
    #("1111111111111111111111111111111", "2", "3"),
    "3",
  )
}
"#,
    );
}

#[test]
fn constant_tuples() {
    assert_js!(
        r#"
const a = "Hello"
const b = 1;
const c = 2.0;
const e = #("bob", "dug")
        "#,
    );

    assert_js!(
        r#"
const e = #(
  "loooooooooooooong", "loooooooooooong", "loooooooooooooong",
  "loooooooooooooong", "loooooooooooong", "loooooooooooooong",
)
"#
    );
}

#[test]
fn case() {
    assert_js!(
        r#"
fn go(a) {
  case a {
    #(2, a) -> a
    #(1, 1) -> 1
    #(a, b) -> a + b
  }
}
"#,
        r#"function go(a) {
  if (a[0] === 2) {
    let a$1 = a[1];
    return a$1;
  } else if (a[0] === 1 && a[1] === 1) {
    return 1;
  } else {
    let a$1 = a[0];
    let b = a[1];
    return a$1 + b;
  }
}
"#
    );
}

#[test]
fn nested_pattern() {
    assert_js!(
        r#"
fn go(x) {
  case x {
    #(2, #(a, b)) -> a + b
    _ -> 1
  }
}
"#,
        r#"function go(x) {
  if (x[0] === 2) {
    let a = x[1][0];
    let b = x[1][1];
    return a + b;
  } else {
    return 1;
  }
}
"#
    );
}
