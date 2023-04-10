use crate::assert_format;

// https://github.com/gleam-lang/gleam/issues/2119
#[test]
fn comment() {
    assert_format!(
        r#"fn main() {
  let greeting = {
    "Hello"
  }

  greeting
}
"#
    );
}
