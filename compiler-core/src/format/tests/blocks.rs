use crate::assert_format;

// https://github.com/gleam-lang/gleam/issues/2119
#[test]
fn assignment() {
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

// https://github.com/gleam-lang/gleam/issues/2131
#[test]
fn comment() {
    assert_format!(
        r#"fn main() {
  wibble(
    // Hello
    { Nil },
  )
}
"#
    );
}

#[test]
fn block_comment() {
    assert_format!(
        r#"fn main() {
  testbldr.demonstrate(
    named: "Hello, this argument is longer to make it all wrap",
    with: {
      // Comment!
      Nil
      Nil
    },
  )
}
"#
    );
}
