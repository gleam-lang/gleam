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
