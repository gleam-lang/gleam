use crate::{assert_js, assert_ts_def};

#[test]
fn tuple() {
    assert_js!(
        r#"
pub fn go() {
  #("1", "2", "3")
}
"#,
    );
}

#[test]
fn tuple1() {
    assert_js!(
        r#"
pub fn go() {
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
fn tuple_typescript() {
    assert_ts_def!(
        r#"
pub fn go() {
  #("1", "2", "3")
}
"#,
    );
}

#[test]
fn tuple_access() {
    assert_js!(
        r#"
pub fn go() {
  #(1, 2).0
}
"#,
    )
}

#[test]
fn tuple_with_block_element() {
    assert_js!(
        r#"
pub fn go() {
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
}

#[test]
fn tuple_with_block_element1() {
    assert_js!(
        r#"
pub fn go() {
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
pub const a = "Hello"
pub const b = 1
pub const c = 2.0
pub const e = #("bob", "dug")
        "#,
    );
}

#[test]
fn constant_tuples1() {
    assert_js!(
        r#"
pub const e = #(
  "loooooooooooooong", "loooooooooooong", "loooooooooooooong",
  "loooooooooooooong", "loooooooooooong", "loooooooooooooong",
)
"#
    );
}

#[test]
fn tuple_formatting_typescript() {
    assert_ts_def!(
        r#"
pub const e = #(
  "a", "a", "a", "a", "a", "a", "a",
  "a", "a", "a", "a", "a", "a", "a",
  "a", "a", "a", "a", "a", "a", "a",
)
"#
    );
}

#[test]
fn case() {
    assert_js!(
        r#"
pub fn go(a) {
  case a {
    #(2, a) -> a
    #(1, 1) -> 1
    #(a, b) -> a + b
  }
}
"#
    );
}

#[test]
fn nested_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    #(2, #(a, b)) -> a + b
    _ -> 1
  }
}
"#
    );
}
