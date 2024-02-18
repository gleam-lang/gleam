use crate::assert_format;

// https://github.com/gleam-lang/gleam/issues/2083
#[test]
fn nested_index_block() {
    assert_format!(
        r#"pub fn main() {
  { #(1, 2).1 }.1
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2083
#[test]
fn index_block() {
    assert_format!(
        r#"pub fn main() {
  {
    1
    #(1, 2)
  }.1
}
"#
    );
}

#[test]
fn tuple_with_last_splittable_arg() {
    assert_format!(
        r#"fn on_attribute_change() -> Dict(String, Decoder(Msg)) {
  dict.from_list([
    #("value", fn(attr) {
      attr
      |> dynamic.int
      |> result.map(Value)
      |> result.map(AttributeChanged)
    }),
  ])
}
"#
    );

    assert_format!(
        r#"pub fn main() {
  #("value", [
    "a long list that needs to be split on multiple lines",
    "another long string",
  ])
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2607
#[test]
fn inner_items_are_not_indented_after_comment() {
    assert_format!(
        r#"fn main() {
  #(
    // Don't indent simple expressions following a comment
    1,
    2,
  )
}
"#
    );

    assert_format!(
        r#"fn main() {
  #(
    // Don't indent binary operations following a comment
    "Should we break up?" <> " (Y/n)",
    "y",
  )
}
"#
    );

    assert_format!(
        r#"fn main() {
  #(
    // Keep nesting even if there is a comment here
    wobble
    |> fun
    |> fun2,
    wabble,
  )
}
"#
    );
}
