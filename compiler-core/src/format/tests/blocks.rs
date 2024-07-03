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

#[test]
fn last_comments_are_not_moved_out_of_blocks() {
    assert_format!(
        r#"fn main() {
  {
    hello
    // Hope I'm not yeeted out of this block!
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  {
    hello
    {
      { hi }
      // Some nested comments
    }
    // At the end of multiple blocks
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case wibble {
    wobble -> {
      1
      // Hope I can stay inside this clause
    }
  }
}
"#
    );
}
