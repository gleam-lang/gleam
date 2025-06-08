use crate::{assert_format, assert_format_rewrite};

#[test]
fn list_with_trailing_comma_is_broken() {
    assert_format_rewrite!(
        "pub fn main() { [ 1, 2, a, ] }",
        r#"pub fn main() {
  [
    1,
    2,
    a,
  ]
}
"#
    );
}

#[test]
fn constant_list_with_trailing_comma_is_broken() {
    assert_format_rewrite!(
        "const list = [ 1, 2, a, ]",
        r#"const list = [
  1,
  2,
  a,
]
"#
    );
}

#[test]
fn list_with_trailing_comma_is_kept_broken() {
    assert_format!(
        r#"pub fn main() {
  [
    1,
    2,
    a,
  ]
}
"#
    );
}

#[test]
fn constant_list_with_trailing_comma_is_kept_broken() {
    assert_format!(
        r#"const list = [
  1,
  2,
  a,
]
"#
    );
}

#[test]
fn list_with_no_trailing_comma_is_packed_on_a_single_line() {
    assert_format_rewrite!(
        r#"pub fn main() {
  [
    1,
    2,
    a
  ]
}
"#,
        r#"pub fn main() {
  [1, 2, a]
}
"#
    );
}

#[test]
fn constant_list_with_no_trailing_comma_is_packed_on_a_single_line() {
    assert_format_rewrite!(
        r#"const list = [
  1,
  2,
  a
]"#,
        "const list = [1, 2, a]\n"
    );
}

#[test]
fn list_with_no_comma_is_packed_on_a_single_line_or_split_one_item_per_line() {
    assert_format_rewrite!(
        "pub fn main() {
  [
    1,
    a,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
    b,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312
  ]
}
",
        "pub fn main() {
  [
    1,
    a,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
    b,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
  ]
}
"
    );
}

#[test]
fn constant_list_with_no_comma_is_packed_on_a_single_line_or_split_one_item_per_line() {
    assert_format_rewrite!(
        "const list = [
  1,
  a,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
  b,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312
]
",
        "const list = [
  1,
  a,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
  b,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
]
"
    );
}

#[test]
fn simple_list_with_no_comma_is_packed_on_a_single_line_or_split_one_item_per_line() {
    assert_format_rewrite!(
        r#"pub fn main() {
  [
    "hello",
    "wibble wobble",
    "these are all simple strings",
    "but the list won't be packed",
    "the formatter will keep",
    "one item",
    "per line",
    "since there's no trailing comma here ->"
  ]
}
"#,
        r#"pub fn main() {
  [
    "hello",
    "wibble wobble",
    "these are all simple strings",
    "but the list won't be packed",
    "the formatter will keep",
    "one item",
    "per line",
    "since there's no trailing comma here ->",
  ]
}
"#
    );
}

#[test]
fn simple_list_with_trailing_comma_and_multiple_items_per_line_is_packed() {
    assert_format_rewrite!(
        r#"pub fn main() {
  [
    "hello",
    "wibble wobble",
    "these are all simple strings",
    "and the list will be packed since the following strings are",
    "on the same", "line", "and there's a trailing comma ->",
  ]
}
"#,
        r#"pub fn main() {
  [
    "hello", "wibble wobble", "these are all simple strings",
    "and the list will be packed since the following strings are", "on the same",
    "line", "and there's a trailing comma ->",
  ]
}
"#
    );
}

#[test]
fn simple_constant_list_with_trailing_comma_and_multiple_items_per_line_is_packed() {
    assert_format_rewrite!(
        r#"pub const list = [
  "hello",
  "wibble wobble",
  "these are all simple strings",
  "and the list will be packed since the following strings are",
  "on the same", "line", "and there's a trailing comma ->",
]
"#,
        r#"pub const list = [
  "hello", "wibble wobble", "these are all simple strings",
  "and the list will be packed since the following strings are", "on the same",
  "line", "and there's a trailing comma ->",
]
"#
    );
}

#[test]
fn simple_packed_list_with_trailing_comma_is_kept_with_multiple_items_per_line() {
    assert_format!(
        r#"pub fn main() {
  [
    "hello", "wibble wobble", "these are all simple strings",
    "and the list will be kept packed since it ends with a trailing comma",
    "right here! ->",
  ]
}
"#
    );
}

#[test]
fn simple_single_line_list_with_trailing_comma_is_split_one_item_per_line() {
    assert_format_rewrite!(
        r#"pub fn main() {
  ["these are all simple strings", "but the list won't be packed", "since it ends with a trailing comma ->",]
}
"#,
        r#"pub fn main() {
  [
    "these are all simple strings",
    "but the list won't be packed",
    "since it ends with a trailing comma ->",
  ]
}
"#
    );
}

#[test]
fn simple_single_line_list_with_no_trailing_comma_is_split_one_item_per_line() {
    assert_format_rewrite!(
        r#"pub fn main() {
  ["these are all simple strings", "but the list won't be packed", "even if it doesn't end with a trailing comma!"]
}
"#,
        r#"pub fn main() {
  [
    "these are all simple strings",
    "but the list won't be packed",
    "even if it doesn't end with a trailing comma!",
  ]
}
"#
    );
}

#[test]
fn empty_lines_in_list_are_not_ignored() {
    assert_format_rewrite!(
        "pub fn main() {
  [1, 2,

  3
  ]
}
",
        "pub fn main() {
  [
    1,
    2,

    3,
  ]
}
"
    );
}

#[test]
fn empty_lines_in_const_list_are_not_ignored() {
    assert_format_rewrite!(
        "const list =
  [1, 2,

  3
  ]
",
        "const list = [
  1,
  2,

  3,
]
"
    );
}

#[test]
fn lists_with_empty_lines_are_always_broken() {
    assert_format_rewrite!(
        "pub fn main() {
  [
    1,
    2,

    3, 4, 5
  ]
}
",
        "pub fn main() {
  [
    1,
    2,

    3,
    4,
    5,
  ]
}
"
    );
}

#[test]
fn const_lists_with_empty_lines_are_always_broken() {
    assert_format_rewrite!(
        "const list =
  [
    1,
    2,

    3, 4, 5
  ]
",
        "const list = [
  1,
  2,

  3,
  4,
  5,
]
"
    );
}
