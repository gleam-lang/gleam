use crate::{assert_format, assert_format_rewrite};

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

// https://github.com/gleam-lang/gleam/issues/2428
#[test]
fn comments_are_not_moved_out_of_blocks() {
    assert_format!(
        r#"pub fn main() {
  function(with: {
    // foo
    2
  })
}
"#
    );
    assert_format!(
        r#"pub fn main() {
  {
    // foo
    2
  }
}
"#
    );

    assert_format_rewrite!(
        r#"pub fn main() {
  { // foo
    2
  }
}
"#,
        r#"pub fn main() {
  {
    // foo
    2
  }
}
"#
    );
}
