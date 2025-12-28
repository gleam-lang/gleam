use crate::{assert_format, assert_format_rewrite};

#[test]
fn construction() {
    assert_format!(
        "fn main() {
  let a = 1
  let x = <<1, a, 2:bytes>>
  let size = <<3:2, 4:size(3), 5:bytes-size(4), 6:size(a)>>
  let unit = <<7:unit(1), 8:bytes-unit(2)>>
  x
}
",
    );
}

#[test]
fn pattern() {
    assert_format!(
        "fn main() {
  let a = 1
  let <<b, c, d:bytes>> = <<1, a, 2:bytes>>
  b
}
",
    );
}

#[test]
fn long() {
    assert_format!(
        "fn main() {
  let some_really_long_variable_name_to_force_wrapping = 1
  let bits = <<
    some_really_long_variable_name_to_force_wrapping,
    some_really_long_variable_name_to_force_wrapping,
  >>
  bits
}
",
    );
}

// https://github.com/gleam-lang/gleam/issues/2932
#[test]
fn tight_empty() {
    assert_format!(
        "fn main() {
  let some_really_really_really_really_really_really_really_long_variable_name_to_force_wrapping = <<>>
  some_really_really_really_really_really_really_really_long_variable_name_to_force_wrapping
}
"
    );
}

#[test]
fn comments_are_not_moved_out_of_empty_bit_array() {
    assert_format!(
        r#"pub fn main() {
  // This is an empty bit array!
  <<
    // Nothing here...
  >>
}
"#
    );
}

#[test]
fn empty_bit_arrays_with_comment_inside_are_indented_properly() {
    assert_format!(
        r#"pub fn main() {
  fun(
    <<
      // Nothing here...
    >>,
    wibble_wobble_wibble_wobble_wibble_wobble_wibble_wobble,
    <<
      // Nothing here as well!
    >>,
  )
}
"#
    );
}

#[test]
fn comments_inside_non_empty_bit_arrays_are_not_moved() {
    assert_format!(
        r#"pub fn main() {
  fun(
    <<
      // One is below me.
      1, 2,
      // Three is below me.
      3,
    >>,
    wibble_wobble_wibble_wobble_wibble_wobble_wibble_wobble,
    <<
      // Three is below me.
      3,
    >>,
  )
}
"#
    );
}

#[test]
fn concise_wrapping_of_simple_bit_arrays() {
    assert_format!(
        "pub fn main() {
  <<
    100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400,
    1500, 1600, 1700, 1800, 1900, 2000,
  >>
}
"
    );
}

#[test]
fn concise_wrapping_of_simple_bit_arrays1() {
    assert_format!(
        "pub fn main() {
  <<
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.0, 11.0, 12.0, 13.0, 14.0,
    15.0, 16.0, 17.0, 18.0, 19.0, 2.0,
  >>
}
"
    );
}

#[test]
fn concise_wrapping_of_simple_bit_arrays2() {
    assert_format!(
        r#"pub fn main() {
  <<
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    "ten", "eleven", "twelve",
  >>
}
"#
    );
}

#[test]
fn concise_wrapping_of_simple_bit_arrays3() {
    assert_format!(
        "const values = <<
  100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400,
  1500, 1600, 1700, 1800, 1900, 2000,
>>
"
    );
}

#[test]
fn concise_wrapping_of_simple_bit_arrays4() {
    assert_format!(
        "const values = <<
  1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.0, 11.0, 12.0, 13.0, 14.0, 15.0,
  16.0, 17.0, 18.0, 19.0, 2.0,
>>
"
    );
}

#[test]
fn concise_wrapping_of_simple_bit_arrays5() {
    assert_format!(
        r#"const values = <<
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
  "eleven", "twelve",
>>
"#
    );
}

#[test]
fn binop_value() {
    assert_format!(
        r#"pub fn main() {
  <<{ 1 + 1 }>>
}
"#
    );
}

#[test]
fn block_value() {
    assert_format!(
        r#"pub fn main() {
  <<
    {
      io.println("hi")
      1
    },
  >>
}
"#
    );
}

#[test]
fn operator_in_pattern_size() {
    assert_format!(
        "pub fn main() {
  let assert <<len, payload:size({ len + 1 } * 8 + 1)>> = <<>>
}
"
    );
}

#[test]
// https://github.com/gleam-lang/gleam/issues/4792#issuecomment-3096177213
fn bit_array_segments_are_kept_one_per_line() {
    assert_format!(
        "pub fn main() {
  <<
    1:1,
    1:1,
    0:2,
    opcode:4,
    masked:1,
    length_section:bits,
    mask_key:bits,
    data:bits,
  >>
  |> bytes_tree.from_bit_array
}
"
    );
}

#[test]
fn bit_array_with_trailing_comma_is_broken() {
    assert_format_rewrite!(
        "pub fn main() { <<1, 2, a,>> }",
        r#"pub fn main() {
  <<
    1,
    2,
    a,
  >>
}
"#
    );
}

#[test]
fn constant_bit_array_with_trailing_comma_is_broken() {
    assert_format_rewrite!(
        "const bit_array = <<1, 2, a,>>",
        r#"const bit_array = <<
  1,
  2,
  a,
>>
"#
    );
}

#[test]
fn bit_array_with_trailing_comma_is_kept_broken() {
    assert_format!(
        r#"pub fn main() {
  <<
    1,
    2,
    a,
  >>
}
"#
    );
}

#[test]
fn constant_bit_array_with_trailing_comma_is_kept_broken() {
    assert_format!(
        r#"const bit_array = <<
  1,
  2,
  a,
>>
"#
    );
}

#[test]
fn bit_array_with_no_trailing_comma_is_packed_on_a_single_line() {
    assert_format_rewrite!(
        r#"pub fn main() {
  <<
    1,
    2,
    a
  >>
}
"#,
        r#"pub fn main() {
  <<1, 2, a>>
}
"#
    );
}

#[test]
fn constant_bit_array_with_no_trailing_comma_is_packed_on_a_single_line() {
    assert_format_rewrite!(
        r#"const bit_array = <<
  1,
  2,
  a
>>"#,
        "const bit_array = <<1, 2, a>>\n"
    );
}

#[test]
fn bit_array_with_no_comma_is_packed_on_a_single_line_or_split_one_item_per_line() {
    assert_format_rewrite!(
        "pub fn main() {
  <<
    1,
    a,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
    b,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312
  >>
}
",
        "pub fn main() {
  <<
    1,
    a,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
    b,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
    12_312_312_312_312_312_312_312,
  >>
}
"
    );
}

#[test]
fn constant_bit_array_with_no_comma_is_packed_on_a_single_line_or_split_one_item_per_line() {
    assert_format_rewrite!(
        "const bit_array = <<
  1,
  a,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
  b,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312
>>
",
        "const bit_array = <<
  1,
  a,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
  b,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
  12_312_312_312_312_312_312_312,
>>
"
    );
}

#[test]
fn simple_bit_array_with_no_comma_is_packed_on_a_single_line_or_split_one_item_per_line() {
    assert_format_rewrite!(
        r#"pub fn main() {
  <<
    "hello",
    "wibble wobble",
    "these are all simple strings",
    "but the bitarray won't be packed",
    "the formatter will keep",
    "one item",
    "per line",
    "since there's no trailing comma here ->"
  >>
}
"#,
        r#"pub fn main() {
  <<
    "hello",
    "wibble wobble",
    "these are all simple strings",
    "but the bitarray won't be packed",
    "the formatter will keep",
    "one item",
    "per line",
    "since there's no trailing comma here ->",
  >>
}
"#
    );
}

#[test]
fn simple_bit_array_with_trailing_comma_and_multiple_items_per_line_is_packed() {
    assert_format_rewrite!(
        r#"pub fn main() {
  <<
    "hello",
    "wibble wobble",
    "these are all simple strings",
    "and the bit array will be packed since the following strings are",
    "on the same", "line", "and there's a trailing comma ->",
  >>
}
"#,
        r#"pub fn main() {
  <<
    "hello", "wibble wobble", "these are all simple strings",
    "and the bit array will be packed since the following strings are",
    "on the same", "line", "and there's a trailing comma ->",
  >>
}
"#
    );
}

#[test]
fn simple_constant_bit_array_with_trailing_comma_and_multiple_items_per_line_is_packed() {
    assert_format_rewrite!(
        r#"pub const bit_array = <<
  "hello",
  "wibble wobble",
  "these are all simple strings",
  "and the bit array will be packed since the following strings are",
  "on the same", "line", "and there's a trailing comma ->",
>>
"#,
        r#"pub const bit_array = <<
  "hello", "wibble wobble", "these are all simple strings",
  "and the bit array will be packed since the following strings are",
  "on the same", "line", "and there's a trailing comma ->",
>>
"#
    );
}

#[test]
fn simple_packed_bit_array_with_trailing_comma_is_kept_with_multiple_items_per_line() {
    assert_format!(
        r#"pub fn main() {
  <<
    "hello", "wibble wobble", "these are all simple strings",
    "and the bit array will be kept packed since it ends with a trailing comma",
    "right here! ->",
  >>
}
"#
    );
}

#[test]
fn simple_single_line_bit_array_with_trailing_comma_is_split_one_item_per_line() {
    assert_format_rewrite!(
        r#"pub fn main() {
  <<"these are all simple strings", "but the bit array won't be packed", "since it ends with a trailing comma ->",>>
}
"#,
        r#"pub fn main() {
  <<
    "these are all simple strings",
    "but the bit array won't be packed",
    "since it ends with a trailing comma ->",
  >>
}
"#
    );
}

#[test]
fn simple_single_line_bit_array_with_no_trailing_comma_is_split_one_item_per_line() {
    assert_format_rewrite!(
        r#"pub fn main() {
  <<"these are all simple strings", "but the bit array won't be packed", "even if it doesn't end with a trailing comma!">>
}
"#,
        r#"pub fn main() {
  <<
    "these are all simple strings",
    "but the bit array won't be packed",
    "even if it doesn't end with a trailing comma!",
  >>
}
"#
    );
}
