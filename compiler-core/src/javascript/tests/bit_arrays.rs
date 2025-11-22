use hexpm::version::Version;
use pubgrub::Range;

use crate::{
    assert_js, assert_js_no_warnings_with_gleam_version, assert_js_warnings_with_gleam_version,
    assert_ts_def,
};

#[test]
fn empty() {
    assert_js!(
        r#"
pub fn go() {
  <<>>
}
"#,
    );
}

#[test]
fn one() {
    assert_js!(
        r#"
pub fn go() {
  <<256>>
}
"#,
    );
}

#[test]
fn two() {
    assert_js!(
        r#"
pub fn go() {
  <<256, 4>>
}
"#,
    );
}

#[test]
fn integer() {
    assert_js!(
        r#"
pub fn go() {
  <<256:int>>
}
"#,
    );
}

#[test]
fn float() {
    assert_js!(
        r#"
pub fn go() {
  <<1.1:float>>
}
"#,
    );
}

#[test]
fn float_big_endian() {
    assert_js!(
        r#"
pub fn go() {
  <<1.1:float-big>>
}
"#,
    );
}

#[test]
fn float_little_endian() {
    assert_js!(
        r#"
pub fn go() {
  <<1.1:float-little>>
}
"#,
    );
}

#[test]
fn float_sized() {
    assert_js!(
        r#"
pub fn go() {
  <<1.1:float-32>>
}
"#,
    );
}

#[test]
fn float_sized_big_endian() {
    assert_js!(
        r#"
pub fn go() {
  <<1.1:float-32-big>>
}
"#,
    );
}

#[test]
fn float_sized_little_endian() {
    assert_js!(
        r#"
pub fn go() {
  <<1.1:float-32-little>>
}
"#,
    );
}

#[test]
fn sized_constant_value() {
    assert_js!(
        r#"
pub fn go() {
  <<256:64>>
}
"#,
    );
}

#[test]
fn sized_dynamic_value() {
    assert_js!(
        r#"
pub fn go(i: Int) {
  <<i:64>>
}
"#,
    );
}

#[test]
fn sized_constant_value_positive_overflow() {
    assert_js!(
        r#"
pub fn go() {
  <<80_000:16>>
}
"#,
    );
}

#[test]
fn sized_constant_value_negative_overflow() {
    assert_js!(
        r#"
pub fn go() {
  <<-80_000:16>>
}
"#,
    );
}

#[test]
fn sized_constant_value_max_size_for_compile_time_evaluation() {
    assert_js!(
        r#"
pub fn go() {
  <<-1:48>>
}
"#,
    );
}

#[test]
fn sized_big_endian_constant_value() {
    assert_js!(
        r#"
pub fn go() {
  <<256:16-big>>
}
"#,
    );
}

#[test]
fn sized_big_endian_dynamic_value() {
    assert_js!(
        r#"
pub fn go(i: Int) {
  <<i:16-big>>
}
"#,
    );
}

#[test]
fn sized_little_endian_constant_value() {
    assert_js!(
        r#"
pub fn go() {
  <<256:16-little>>
}
"#,
    );
}

#[test]
fn sized_little_endian_dynamic_value() {
    assert_js!(
        r#"
pub fn go(i: Int) {
  <<i:16-little>>
}
"#,
    );
}

#[test]
fn explicit_sized_constant_value() {
    assert_js!(
        r#"
pub fn go() {
  <<256:size(32)>>
}
"#,
    );
}

#[test]
fn explicit_sized_dynamic_value() {
    assert_js!(
        r#"
pub fn go(i: Int) {
  <<i:size(32)>>
}
"#,
    );
}

#[test]
fn variable_sized() {
    assert_js!(
        r#"
pub fn go(x, y) {
  <<x:size(y)>>
}
"#,
    );
}

#[test]
fn variable() {
    assert_js!(
        r#"
pub fn go(x) {
  <<256, 4, x>>
}
"#,
    );
}

#[test]
fn utf8() {
    assert_js!(
        r#"
pub fn go(x) {
  <<256, 4, x, "Gleam":utf8>>
}
"#,
    );
}

#[test]
fn match_utf8_with_escape_chars() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<"\"\\\r\n\t\f\u{1f600}">> = x
}
"#,
    );
}

#[test]
fn match_utf8() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<"Gleam 👍":utf8>> = x
}
"#,
    );
}

#[test]
fn match_case_utf8_with_escape_chars() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<"\"\\\r\n\t\f\u{1f600}">> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_case_utf8() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<"Gleam 👍":utf8>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn utf8_codepoint() {
    assert_js!(
        r#"
pub fn go(x) {
  <<x:utf8_codepoint, "Gleam":utf8>>
}
"#,
    );
}

#[test]
fn utf8_codepoint_typescript() {
    assert_ts_def!(
        r#"
pub fn go(x) {
  <<x:utf8_codepoint, "Gleam":utf8>>
}
"#,
    );
}

#[test]
fn bit_string() {
    assert_js!(
        r#"
pub fn go(x) {
  <<x:bits>>
}
"#,
    );
}

#[test]
fn bits() {
    assert_js!(
        r#"
pub fn go(x) {
  <<x:bits>>
}
"#,
    );
}

#[test]
fn bit_array_sliced() {
    assert_js!(
        r#"
pub fn go(x) {
  <<<<0xAB>>:bits-4>>
}
"#,
    );
}

#[test]
fn bit_array_dynamic_slice() {
    assert_js!(
        r#"
pub fn go(x) {
  let i = 4
  <<<<0xAB>>:bits-size(i)>>
}
"#,
    );
}

#[test]
fn bit_string_typescript() {
    assert_ts_def!(
        r#"
pub fn go(x) {
  <<x:bits>>
}
"#,
    );
}

#[test]
fn bits_typescript() {
    assert_ts_def!(
        r#"
pub fn go(x) {
  <<x:bits>>
}
"#,
    );
}

#[test]
fn empty_match() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<>> = x
}
"#,
    );
}

#[test]
fn case_empty_match() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_bytes() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<1, y>> = x
}
"#,
    );
}

#[test]
fn case_match_bytes() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<1, y>> -> y
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn match_sized() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:16, b:8>> = x
}
"#,
    );
}

#[test]
fn case_match_sized() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:16, b:8>> -> a + b
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn match_sized_unaligned() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:17, b:7>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_unaligned() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:17, b:7>> -> b * 2
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn match_sized_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<1234:16, 123:8>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<1234:16, 123:8>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_unsigned() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:unsigned>> = x
}
"#,
    );
}

#[test]
fn case_match_unsigned() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:unsigned>> -> a
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn match_unsigned_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<-2:unsigned>> = x
}
"#,
    );
}

#[test]
fn case_match_unsigned_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<-2:unsigned>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_signed() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:signed>> = x
}
"#,
    );
}

#[test]
fn case_match_signed() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:signed>> -> a
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn match_signed_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<-1:signed>> = x
}
"#,
    );
}

#[test]
fn case_match_signed_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<-1:signed>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_sized_big_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:16-big>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_big_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:16-big>> -> a
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn match_sized_big_endian_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<1234:16-big>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_big_endian_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<1234:16-big>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_sized_little_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:16-little>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_little_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:16-little>> -> a
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn match_sized_little_endian_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<1234:16-little>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_little_endian_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<1234:16-little>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_sized_big_endian_unsigned() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:16-big-unsigned>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_big_endian_unsigned() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:16-big-unsigned>> -> a
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn match_sized_big_endian_unsigned_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<1234:16-big-unsigned>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_big_endian_unsigned_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<1234:16-big-unsigned>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_sized_big_endian_signed() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:16-big-signed>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_big_endian_signed() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:16-big-signed>> -> a
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn match_sized_big_endian_signed_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<1234:16-big-signed>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_big_endian_signed_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<1234:16-big-signed>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_sized_little_endian_unsigned() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:16-little-unsigned>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_little_endian_unsigned() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:16-little-unsigned>> -> a
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn match_sized_little_endian_unsigned_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<1234:16-little-unsigned>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_little_endian_unsigned_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<1234:16-little-unsigned>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_sized_little_endian_signed() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:16-little-signed>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_little_endian_signed() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:16-little-signed>> -> a
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn match_sized_little_endian_signed_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<1234:16-little-signed>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_little_endian_signed_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<1234:16-little-signed>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_dynamic_size() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 16
  let assert <<a:size(n)>> = x
}
"#
    );
}

#[test]
fn case_match_dynamic_size() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 16
  case x {
    <<a:size(n)>> -> a
    _ -> 1
  }
}
"#
    );
}

#[test]
fn match_dynamic_size_with_other_segments() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 16
  let m = 32
  let assert <<first:size(8), a:size(n), b:size(m), rest:bits>> = x
}
"#
    );
}

#[test]
fn case_match_dynamic_size_with_other_segments() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 16
  let m = 32
  case x {
    <<first:size(8), a:size(n), b:size(m), rest:bits>> -> first + a + b
    _ -> 1
  }
}
"#
    );
}

#[test]
fn match_dynamic_size_shadowed_variable() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 16
  let n = 5
  let assert <<a:size(n)>> = x
}
"#
    );
}

#[test]
fn case_match_dynamic_size_shadowed_variable() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 16
  let n = 5
  case x {
    <<a:size(n)>> -> a
    _ -> 1
  }
}
"#
    );
}

#[test]
fn match_dynamic_size_literal_value() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 8
  let assert <<a:size(n), 0b010101:size(8)>> = x
}
"#
    );
}

#[test]
fn case_match_dynamic_size_literal_value() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 8
  case x {
    <<a:size(n), 0b010101:size(8)>> -> a
    _ -> 1
  }
}
"#
    );
}

#[test]
fn match_dynamic_bits_size() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 16
  let assert <<a:bits-size(n)>> = x
}
"#
    );
}

#[test]
fn case_match_dynamic_bits_size() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 16
  case x {
    <<a:bits-size(n)>> -> a
    _ -> x
  }
}
"#
    );
}

#[test]
fn match_dynamic_bytes_size() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 3
  let assert <<a:bytes-size(n)>> = x
}
"#
    );
}

#[test]
fn case_match_dynamic_bytes_size() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 3
  case x {
    <<a:bytes-size(n)>> -> a
    _ -> x
  }
}
"#
    );
}

#[test]
fn discard_sized() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<_:16, _:8>> = x
  let assert <<_:16-little-signed, _:8>> = x
}
"#,
    );
}

#[test]
fn case_discard_sized() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:16, _:8>> -> 1
    _ -> 2
  }
  case x {
    <<_:16-little-signed, _:8>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_sized_value() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<i:16>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_value() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<i:16>> -> i
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn match_sized_value_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<258:16>> = x
}
"#,
    );
}

#[test]
fn case_match_sized_value_constant_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<258:16>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_float() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:float, b:int>> = x
}
"#,
    );
}

#[test]
fn case_match_float() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:float, b:int>> -> #(a, b)
    _ -> #(1.1, 2)
  }
}
"#,
    );
}

#[test]
fn match_float_big_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:float-big, b:int>> = x
}
"#,
    );
}

#[test]
fn case_match_float_big_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:float-big, b:int>> -> #(a, b)
    _ -> #(1.1, 1)
  }
}
"#,
    );
}

#[test]
fn match_float_little_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:float-little, b:int>> = x
}
"#,
    );
}

#[test]
fn case_match_float_little_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:float-little, b:int>> -> #(a, b)
    _ -> #(1.1, 2)
  }
}
"#,
    );
}

#[test]
fn match_float_sized() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:float-32, b:int>> = x
}
"#,
    );
}

#[test]
fn case_match_float_sized() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:float-32, b:int>> -> #(a, b)
    _ -> #(1.1, 2)
  }
}
"#,
    );
}

#[test]
fn match_float_sized_big_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:float-32-big, b:int>> = x
}
"#,
    );
}

#[test]
fn case_match_float_sized_big_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:float-32-big, b:int>> -> #(a, b)
    _ -> #(1.1, 2)
  }
}
"#,
    );
}

#[test]
fn match_float_sized_little_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:float-32-little, b:int>> = x
}
"#,
    );
}

#[test]
fn case_match_float_sized_little_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:float-32-little, b:int>> -> #(a, b)
    _ -> #(1.1, 2)
  }
}
"#,
    );
}

#[test]
fn match_literal_float() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<1.4, b:int>> = x
}
"#,
    );
}

#[test]
fn case_match_literal_float() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<1.4, b:int>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_literal_unaligned_float() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 1
  let assert <<_:size(n), 1.1, _:bits>> = x
}
"#,
    );
}

#[test]
fn case_match_literal_unaligned_float() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 1
  case x {
    <<_:size(n), 1.1, _:int>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_literal_aligned_float() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<_, 1.1, _:bits>> = x
}
"#,
    );
}

#[test]
fn case_match_literal_aligned_float() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_, 1.1, _:int>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn match_float_16_bit() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<a:float-size(16)>> = x
}
"#
    );
}

#[test]
fn case_match_float_16_bit() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<a:float-size(16)>> -> a
    _ -> 1.1
  }
}
"#
    );
}

#[test]
fn match_rest() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<_, b:bytes>> = <<1,2,3>>
}
"#,
    );
}

#[test]
fn case_match_rest() {
    assert_js!(
        r#"
pub fn go(x) {
  case <<1, 2, 3>> {
    <<_, b:bytes>> -> b
    _ -> x
  }
}
"#,
    );
}

#[test]
fn match_bytes_with_size() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<f:bytes-2>> = <<1, 2>>
}
"#,
    );
}

#[test]
fn case_match_bytes_with_size() {
    assert_js!(
        r#"
pub fn go(x) {
  case <<1, 2>> {
    <<f:bytes-2>> -> f
    _ -> x
  }
}
"#,
    );
}

#[test]
fn match_bits_with_size() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<_:4, f:bits-2, _:1>> = <<0x77:7>>
}
"#,
    );
}

#[test]
fn case_match_bits_with_size() {
    assert_js!(
        r#"
pub fn go(x) {
  case <<0x77:7>> {
    <<_:4, f:bits-2, _:1>> -> f
    _ -> x
  }
}
"#,
    );
}

#[test]
fn match_rest_bytes() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<_, b:bytes>> = <<1,2,3>>
}
"#,
    );
}

#[test]
fn case_match_rest_bytes() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_, b:bytes>> -> b
    _ -> x
  }
}
"#,
    );
}

#[test]
fn match_rest_bits() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<_, b:bits>> = <<1,2,3>>
}
"#,
    );
}

#[test]
fn case_match_rest_bits() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_, b:bits>> -> b
    _ -> x
  }
}
"#,
    );
}

#[test]
fn match_rest_bits_unaligned() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<_:5, b:bits>> = <<1,2,3>>
}
"#,
    );
}

#[test]
fn case_match_rest_bits_unaligned() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:5, b:bits>> -> b
    _ -> x
  }
}
"#,
    );
}

#[test]
fn match_binary_size() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<_, a:2-bytes>> = x
  let assert <<_, b:bytes-size(2)>> = x
}
"#,
    );
}

#[test]
fn case_match_binary_size() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_, a:2-bytes>> -> a
    _ -> x
  }

  case x {
    <<_, b:bytes-size(2)>> -> b
    _ -> x
  }
}
"#,
    );
}

#[test]
fn unaligned_int_expression_requires_v1_9() {
    assert_js_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 8, 0)),
        "
pub fn main() {
  <<0:1>>
}
  ",
    );
}

#[test]
fn bits_expression_does_not_require_v1_9() {
    assert_js_no_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 8, 0)),
        "
pub fn main() {
  <<<<0>>:bits>>
}
  ",
    );
}

#[test]
fn sized_bits_expression_requires_v1_9() {
    assert_js_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 8, 0)),
        "
pub fn main() {
  <<<<0>>:bits-5>>
}
  ",
    );
}

#[test]
fn unaligned_int_pattern_requires_v1_9() {
    assert_js_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 8, 0)),
        "
pub fn main() {
  let assert <<_:3>> = <<0>>
}
  ",
    );
}

#[test]
fn bits_pattern_requires_v1_9() {
    assert_js_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 8, 0)),
        "
pub fn main() {
  let assert <<_:bits>> = <<0>>
}
  ",
    );
}

#[test]
fn bytes_pattern_with_odd_size_does_not_require_v1_9() {
    assert_js_no_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 8, 0)),
        "
pub fn main() {
  let assert <<_:bytes-3>> = <<0, 1, 2>>
}
  ",
    );
}

#[test]
fn sized_bits_expression_with_javascript_external_does_not_require_v1_9() {
    assert_js_no_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 8, 0)),
        r#"
@external(javascript, "test.mjs", "go")
pub fn go() -> BitArray {
  <<0:size(5)>>
}
  "#,
    );
}

#[test]
fn bits_pattern_with_javascript_external_does_not_require_v1_9() {
    assert_js_no_warnings_with_gleam_version!(
        Range::higher_than(Version::new(1, 8, 0)),
        r#"
@external(javascript, "test.mjs", "go")
pub fn go() -> BitArray {
  let <<a:bits>> = <<>>
  a
}
  "#,
    );
}

#[test]
fn as_module_const() {
    assert_js!(
        r#"
          pub const data = <<
            0x1,
            2,
            2:size(16),
            0x4:size(32),
            -1:32,
            "Gleam":utf8,
            4.2:float,
            4.2:32-float,
            <<0xFA>>:bits-6,
            -1:64,
            <<
              <<1, 2, 3>>:bits,
              "Gleam":utf8,
              1024
            >>:bits
          >>
        "#
    )
}

#[test]
fn bit_array_literal_string_constant_is_treated_as_utf8() {
    assert_js!(r#"pub const a = <<"hello", " ", "world">>"#);
}

#[test]
fn bit_array_literal_string_is_treated_as_utf8() {
    assert_js!(
        r#"
pub fn main() {
  <<"hello", " ", "world">>
}"#
    );
}

#[test]
fn bit_array_literal_string_pattern_is_treated_as_utf8() {
    assert_js!(
        r#"
pub fn main() {
  case <<>> {
    <<"a", "b", _:bytes>> -> 1
    _ -> 2
  }
}"#
    );
}

#[test]
fn with_unit() {
    assert_js!(
        r#"
pub fn main() {
  <<1:size(2)-unit(2), 2:size(3)-unit(4)>>
}
"#,
    );
}

#[test]
fn dynamic_size_with_unit() {
    assert_js!(
        r#"
pub fn main() {
  let size = 3
  <<1:size(size)-unit(2)>>
}
"#,
    );
}

#[test]
fn pattern_with_unit() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<1:size(2)-unit(2), 2:size(3)-unit(4)>> = x
}
"#,
    );
}

#[test]
fn case_pattern_with_unit() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<1:size(2)-unit(2), 2:size(3)-unit(4)>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn dynamic_size_pattern_with_unit() {
    assert_js!(
        r#"
pub fn go(x) {
  let size = 3
  let assert <<1:size(size)-unit(2)>> = x
}
"#,
    );
}

#[test]
fn case_dynamic_size_pattern_with_unit() {
    assert_js!(
        r#"
pub fn go(x) {
  let size = 3
  case x {
    <<1:size(size)-unit(2)>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn case_dynamic_size_float_pattern_with_unit() {
    assert_js!(
        r#"
pub fn go(x) {
  let size = 3
  case x {
    <<1.3:size(size)-unit(2)>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn case_with_remaining_bytes_after_constant_size() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_, _, _:bytes>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn case_with_remaining_bytes_after_variable_size() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 1
  case x {
    <<_:size(n), _, _:bytes>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn case_with_remaining_bytes_after_variable_size_2() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 1
  case x {
    <<m:size(n), _:size(m), _:bytes>> -> 1
    _ -> 2
  }
}
"#,
    );
}

#[test]
fn case_is_byte_aligned() {
    assert_js!(
        r#"
pub fn is_byte_aligned(x) {
  case x {
    <<_:bytes>> -> True
    _ -> False
  }
}
"#,
    );
}

#[test]
fn alternative_patterns_with_variable_size() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_, n, rest:size(n)>> |
    <<n, _, rest:size(n)>> -> True
    _ -> False
  }
}
"#,
    );
}

#[test]
fn variable_sized_segment() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<n, rest:size(n)>> -> 1
    _ -> 2
  }
}
"#
    )
}

#[test]
fn segments_shadowing_each_other() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = 1
  case x {
    <<n, rest:size(n)>> -> 1
    _ -> 2
  }
}
"#
    )
}

#[test]
fn negative_size_pattern() {
    assert_js!(
        r#"
pub fn go(x) {
  let n = -10
  case x {
    <<int:size(n)>> -> int
    _ -> 2
  }
}
"#
    )
}

#[test]
fn negative_size_pattern_2() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<n:signed, int:size(n)>> -> int
    _ -> 2
  }
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/3375
#[test]
fn bit_array_assignment_int() {
    assert_js!(
        "
pub fn main() {
 let assert <<1 as a>> = <<1>>
 a
}
"
    );
}

#[test]
fn case_bit_array_assignment_int() {
    assert_js!(
        "
pub fn go(x) {
 case x {
    <<1 as n>>
    | <<2 as n, _:bytes>> -> n
    _ -> 1
 }
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/3375
#[test]
fn bit_array_assignment_float() {
    assert_js!(
        "
pub fn main() {
 let assert <<3.14 as pi:float>> = <<3.14>>
 pi
}
"
    );
}

#[test]
fn case_bit_array_assignment_float() {
    assert_js!(
        "
pub fn go(x) {
 case x {
    <<3.14 as pi:float>>
    | <<1.1 as pi:float, _:bytes>> -> pi
    _ -> 1.1
 }
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/3375
#[test]
fn bit_array_assignment_string() {
    assert_js!(
        r#"
pub fn main() {
 let assert <<"Hello, world!" as message:utf8>> = <<"Hello, world!">>
 message
}
"#
    );
}

#[test]
fn case_bit_array_assignment_string() {
    assert_js!(
        r#"
pub fn go(x) {
 case x {
    <<"Hello" as message>>
    | <<"Jak" as message, _:bytes>> -> message
    _ -> "wibble"
 }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3375
#[test]
fn bit_array_assignment_discard() {
    assert_js!(
        r#"
pub fn main() {
 let assert <<_ as number>> = <<10>>
 number
}
"#
    );
}

#[test]
fn case_bit_array_assignment_discard() {
    assert_js!(
        r#"
pub fn go(x) {
 case x {
    <<_ as n>>
    | <<_ as n, _:bytes>> -> n
    _ -> 1
 }
}
"#
    );
}

#[test]
fn utf16() {
    assert_js!(
        r#"
pub fn main() {
  <<"Hello, world!":utf16>>
}
"#
    );
}

#[test]
fn utf16_codepoint() {
    assert_js!(
        r#"
fn codepoint() -> UtfCodepoint { todo }

pub fn main() {
  let my_codepoint = codepoint()
  <<my_codepoint:utf16_codepoint>>
}
"#
    );
}

#[test]
fn utf32() {
    assert_js!(
        r#"
pub fn main() {
  <<"Hello, world!":utf32>>
}
"#
    );
}

#[test]
fn utf32_codepoint() {
    assert_js!(
        r#"
fn codepoint() -> UtfCodepoint { todo }

pub fn main() {
  let my_codepoint = codepoint()
  <<my_codepoint:utf32_codepoint>>
}
"#
    );
}

#[test]
fn const_utf16() {
    assert_js!(
        r#"
pub const message = <<"Hello, world!":utf16>>
"#
    );
}

#[test]
fn const_utf32() {
    assert_js!(
        r#"
pub const message = <<"Hello, world!":utf32>>
"#
    );
}

#[test]
fn pattern_match_utf16() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<"Hello":utf16, _rest:bytes>> = x
}
"#
    );
}

#[test]
fn pattern_match_utf32() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<"Hello":utf32, _rest:bytes>> = x
}
"#
    );
}

#[test]
fn utf16_little_endian() {
    assert_js!(
        r#"
pub fn main() {
  <<"Hello, world!":utf16-little>>
}
"#
    );
}

#[test]
fn utf32_little_endian() {
    assert_js!(
        r#"
pub fn main() {
  <<"Hello, world!":utf32-little>>
}
"#
    );
}

#[test]
fn pattern_match_utf16_little_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<"Hello":utf16-little, _rest:bytes>> = x
}
"#
    );
}

#[test]
fn pattern_match_utf32_little_endian() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert <<"Hello":utf32-little, _rest:bytes>> = x
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/4630
#[test]
fn tuple_bit_array() {
    assert_js!(
        "
pub fn go(x) {
  let assert #(<<>>) = x
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4630
#[test]
fn tuple_bit_array_case() {
    assert_js!(
        "
pub fn go(x) {
  case x {
    #(<<>>) -> 1
    _ -> 2
  }
}
"
    );
}

#[test]
fn tuple_multiple_bit_arrays() {
    assert_js!(
        "
pub fn go(x) {
  let assert #(<<>>, <<1>>, <<2, 3>>) = x
}
"
    );
}

#[test]
fn tuple_multiple_bit_arrays_case() {
    assert_js!(
        "
pub fn go(x) {
  case x {
    #(<<>>, <<1>>, <<2, 3>>) -> True
    _ -> False
  }
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4637
#[test]
fn pattern_matching_on_32_float_plus_infinity_still_reachable() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:32-float>> -> "Float"
    <<0x7f800000:32>> -> "+Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_32_float_plus_infinity_still_reachable_2() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:32-float>> -> "Float"
    <<0x7f80:16, 0x0000:16>> -> "+Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_32_float_minus_infinity_still_reachable() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:32-float>> -> "Float"
    <<0xff800000:32>> -> "-Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_32_float_minus_infinity_still_reachable_2() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:32-float>> -> "Float"
    <<0xff80:16, 0x0000:16>> -> "-Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_32_float_nan_still_reachable() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:32-float>> -> "Float"
    <<0x7fc00000:32>> -> "NaN"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_32_float_nan_still_reachable_2() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:32-float>> -> "Float"
    <<0x7fc0:16, 0x0000:16>> -> "NaN"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_plus_infinity_still_reachable() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<0x7ff0000000000000:64>> -> "+Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_plus_infinity_still_reachable_2() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<0x7ff00000:32, 0x00000000:32>> -> "+Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_minus_infinity_still_reachable() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<0xfff0000000000000:64>> -> "-Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_minus_infinity_still_reachable_2() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<0xfff00000:32, 0x00000000:32>> -> "-Infinity"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_nan_still_reachable() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<0x7ff8000000000000:64>> -> "NaN"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_nan_still_reachable_2() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<0x7ff80000:32, 0x00000000:32>> -> "NaN"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_int_is_still_reachable() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<_:64-int>> -> "Int"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn pattern_matching_on_64_float_float_is_unreachable() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    <<_:64-float>> -> "Float"
    <<_:64-float>> -> "unreachable"
    _ -> "Other"
  }
}
"#
    );
}

#[test]
fn unit_with_bits_option() {
    assert_js!(
        "
pub fn go(x) {
  <<x:bits-size(4)-unit(8)>>
}
"
    );
}

#[test]
fn unit_with_bits_option_constant() {
    assert_js!(
        "
pub const bits = <<1, 2, 3>>
pub const more_bits = <<bits:bits-size(3)-unit(8)>>
"
    );
}

#[test]
fn operator_in_size_for_bit_array_segment() {
    assert_js!(
        "
pub fn go(x) {
  <<x:bits-size(4 + 1)>>
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4712
#[test]
fn multiple_variable_size_segments() {
    assert_js!(
        "
pub fn main() {
  let assert <<a, b:size(a), c:size(b)>> = <<1, 2, 3, 4>>
  a + b + c
}
"
    );
}

#[test]
fn utf16_codepoint_little_endian() {
    assert_js!(
        "
pub fn go(codepoint) {
  <<codepoint:utf16_codepoint-little>>
}
"
    );
}

#[test]
fn utf32_codepoint_little_endian() {
    assert_js!(
        "
pub fn go(codepoint) {
  <<codepoint:utf32_codepoint-little>>
}
"
    );
}

#[test]
fn operator_in_pattern_size() {
    assert_js!(
        "
pub fn main() {
  let assert <<len, payload:size(len * 8)>> = <<>>
}
"
    );
}

#[test]
fn operator_in_pattern_size2() {
    assert_js!(
        "
pub fn main() {
  let assert <<len, payload:size(len / 8 - 1)>> = <<>>
}
"
    );
}

#[test]
fn operator_in_pattern_size3() {
    assert_js!(
        "
pub fn main() {
  let additional = 10
  let assert <<len, payload:size(len + additional * 8)>> = <<>>
}
"
    );
}

#[test]
fn block_in_pattern_size() {
    assert_js!(
        "
pub fn main() {
  let assert <<len, payload:size({ len - 1 } * 8)>> = <<>>
}
"
    );
}

#[test]
fn non_byte_aligned_size_calculation() {
    assert_js!(
        "
pub fn main() {
  case <<>> {
    <<a:1, b:3, c:size(b - 2)>> -> c + b
    _ -> 1
  }
}
"
    );
}

#[test]
fn pattern_match_on_negative_size_calculation() {
    assert_js!(
        "
pub fn main() {
  let assert <<a, b:size(a - 100000), c:size(b)>> = <<1, 2, 3, 4, 5>>
}
"
    );
}

#[test]
fn pattern_match_unknown_size_and_literal_string() {
    assert_js!(
        r#"
pub fn go(x, n) {
  case x {
    <<_:size(n), "\r\n">> -> 1
    _ -> 2
  }
}
"#
    );
}

#[test]
fn pattern_match_size_arithmetic() {
    assert_js!(
        r#"
pub fn wibble(bits, wobble) {
  case bits {
    <<_:size(1), _:size(wobble - 1), _:bits>> -> 0
    _ -> 1
  }
}
"#
    );
}
