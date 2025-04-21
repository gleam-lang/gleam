use crate::assert_format;

#[test]
fn one() {
    assert_format!(
        "pub type Counter {
  Counter(a: Int, b: Int)
}

fn main() {
  let c = Counter(0, 0)
  let c = Counter(..c, a: c.a + 1, b: c.a + c.b)
  c
}
"
    );
}

#[test]
fn two() {
    // Long record updates are split onto multiple lines
    assert_format!(
        "pub type Counter {
  Counter(loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong: Int)
}

fn main() {
  let c = Counter(0)
  let c =
    Counter(
      ..c,
      loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong: 1,
    )
  c
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/1872
#[test]
fn comment_before_spread() {
    assert_format!(
        r#"fn main() {
  Thingy(
    // Def?
    // Def!
    ..thingy.defaults,
    one: One,
  )
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1872
#[test]
fn comment_before_update_label() {
    assert_format!(
        r#"fn main() {
  Thingy(
    ..thingy.defaults,
    // Def?
    // Def!
    one: One,
  )
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1872
#[test]
fn multiple_line_custom_type_field_comments() {
    assert_format!(
        r#"fn main() {
  Thingy(
    // Def?
    // Def!
    ..thingy.defaults,
    // One?
    // One!
    one: One,
    // Two?
    // Two!
    two: Two,
  )
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/4120
#[test]
fn record_update_gets_formatted_like_a_function_call() {
    assert_format!(
        r#"pub fn example() {
  Record(..record, field: {
    use _ <- list.map(record.field)
    io.print("Example")
  })
}
"#
    );
}
