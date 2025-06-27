use crate::assert_format;

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
