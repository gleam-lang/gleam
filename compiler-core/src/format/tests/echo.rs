use crate::{assert_format, assert_format_rewrite};

#[test]
fn echo() {
    assert_format!(
        "fn main() {
  echo
}
"
    );
}

#[test]
fn echo_with_value() {
    assert_format!(
        r#"fn main() {
  echo value
}
"#
    );
}

#[test]
fn echo_with_big_value_that_needs_to_be_split() {
    assert_format!(
        r#"fn main() {
  echo [
    this_is_a_long_list_and_requires_splitting,
    wibble_wobble_woo,
    multiple_lines,
  ]
}
"#
    );
}

#[test]
fn echo_inside_a_pipeline() {
    assert_format!(
        r#"fn main() {
  wibble
  |> echo
  |> wobble
}
"#
    );
}

#[test]
fn echo_inside_a_single_line_pipeline() {
    assert_format!(
        r#"fn main() {
  wibble |> echo |> wobble
}
"#
    );
}

#[test]
fn echo_as_last_item_of_pipeline() {
    assert_format!(
        r#"fn main() {
  wibble |> wobble |> echo
}
"#
    );
}

#[test]
fn echo_as_last_item_of_multiline_pipeline() {
    assert_format!(
        r#"fn main() {
  wibble
  |> wobble
  |> echo
}
"#
    );
}

#[test]
fn echo_with_related_expression_on_following_line() {
    assert_format_rewrite!(
        r#"fn main() {
  panic as echo
  "wibble"
}
"#,
        r#"fn main() {
  panic as echo "wibble"
}
"#
    );
}

#[test]
fn echo_with_following_value_in_a_pipeline() {
    assert_format_rewrite!(
        r#"fn main() {
  []
  |> echo wibble
  |> wobble
}
"#,
        r#"fn main() {
  []
  |> echo
  wibble
  |> wobble
}
"#
    );
}

#[test]
fn echo_printing_multiline_pipeline() {
    assert_format_rewrite!(
        r#"fn main() {
  echo first
  |> wibble
  |> wobble
}
"#,
        r#"fn main() {
  echo first
    |> wibble
    |> wobble
}
"#
    );
}

#[test]
fn echo_printing_one_line_pipeline() {
    assert_format!(
        r#"fn main() {
  echo first |> wibble |> wobble
}
"#
    );
}

#[test]
fn echo_as() {
    assert_format!(
        "fn main() {
  echo as hello
}
"
    );
}

#[test]
fn echo_as_with_value() {
    assert_format!(
        r#"fn main() {
  echo value as message
}
"#
    );
}

#[test]
fn echo_as_with_big_value_that_needs_to_be_split() {
    assert_format!(
        r#"fn main() {
  echo call([
    this_is_a_long_list_and_requires_splitting,
    wibble_wobble_woo,
    multiple_lines,
  ])
    as "tag!"
}
"#
    );
}

#[test]
fn echo_as_inside_a_pipeline() {
    assert_format!(
        r#"fn main() {
  wibble
  |> echo as "echooo o o"
  |> wobble
}
"#
    );
}

#[test]
fn echo_as_inside_a_single_line_pipeline() {
    assert_format!(
        r#"fn main() {
  wibble |> echo as string |> wobble
}
"#
    );
}

#[test]
fn echo_as_as_last_item_of_pipeline() {
    assert_format!(
        r#"fn main() {
  wibble |> wobble |> echo as end
}
"#
    );
}

#[test]
fn echo_as_as_last_item_of_multiline_pipeline() {
    assert_format!(
        r#"fn main() {
  wibble
  |> wobble
  |> echo as message
}
"#
    );
}

#[test]
fn echo_as_with_related_expression_on_following_line() {
    assert_format_rewrite!(
        r#"fn main() {
  panic as echo
  "wibble"
  as wobble
}
"#,
        r#"fn main() {
  panic as echo "wibble" as wobble
}
"#
    );
}

#[test]
fn echo_as_with_following_value_in_a_pipeline() {
    assert_format_rewrite!(
        r#"fn main() {
  []
  |> echo as wibble wibble
  |> wobble
}
"#,
        r#"fn main() {
  []
  |> echo as wibble
  wibble
  |> wobble
}
"#
    );
}

#[test]
fn echo_as_printing_multiline_pipeline() {
    assert_format_rewrite!(
        r#"fn main() {
  echo first
  |> wibble
  |> wobble
  as "pipeline"
}
"#,
        r#"fn main() {
  echo first
    |> wibble
    |> wobble
    as "pipeline"
}
"#
    );
}

#[test]
fn echo_as_printing_one_line_pipeline() {
    assert_format!(
        r#"fn main() {
  echo first |> wibble |> wobble as "pipeline"
}
"#
    );
}

#[test]
fn echo_as_with_multiline_message() {
    assert_format!(
        r#"fn main() {
  echo [wibble, wobble]
    as {
      // Force this block to split
      "wibble" <> wobble()
    }
}
"#
    );
}
