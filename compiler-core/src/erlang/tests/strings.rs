use crate::assert_erl;

#[test]
fn concat() {
    assert_erl!(
        r#"
pub fn go(x, y) {
  x <> y
}
"#,
    );
}

#[test]
fn concat_3_variables() {
    assert_erl!(
        r#"
pub fn go(x, y, z) {
  x <> y <> z
}
"#,
    );
}

#[test]
fn string_prefix() {
    assert_erl!(
        r#"
pub fn go(x) {
  case x {
    "Hello, " <> name -> name
    _ -> "Unknown"
  }
}
"#,
    );
}
