use hexpm::version::Version;
use pubgrub::range::Range;

use crate::{
    assert_js, assert_js_error, assert_js_no_warnings_with_gleam_version,
    assert_js_warnings_with_gleam_version, assert_ts_def,
};

#[test]
fn empty() {
    assert_js!(
        r#"
fn go() {
  <<>>
}
"#,
    );
}

#[test]
fn one() {
    assert_js!(
        r#"
fn go() {
  <<256>>
}
"#,
    );
}

#[test]
fn two() {
    assert_js!(
        r#"
fn go() {
  <<256, 4>>
}
"#,
    );
}

#[test]
fn integer() {
    assert_js!(
        r#"
fn go() {
  <<256:int>>
}
"#,
    );
}

#[test]
fn float() {
    assert_js!(
        r#"
fn go() {
  <<1.1:float>>
}
"#,
    );
}

#[test]
fn float_big_endian() {
    assert_js!(
        r#"
fn go() {
  <<1.1:float-big>>
}
"#,
    );
}

#[test]
fn float_little_endian() {
    assert_js!(
        r#"
fn go() {
  <<1.1:float-little>>
}
"#,
    );
}

#[test]
fn float_sized() {
    assert_js!(
        r#"
fn go() {
  <<1.1:float-32>>
}
"#,
    );
}

#[test]
fn float_sized_big_endian() {
    assert_js!(
        r#"
fn go() {
  <<1.1:float-32-big>>
}
"#,
    );
}

#[test]
fn float_sized_little_endian() {
    assert_js!(
        r#"
fn go() {
  <<1.1:float-32-little>>
}
"#,
    );
}

#[test]
fn sized_constant_value() {
    assert_js!(
        r#"
fn go() {
  <<256:64>>
}
"#,
    );
}

#[test]
fn sized_dynamic_value() {
    assert_js!(
        r#"
fn go(i: Int) {
  <<i:64>>
}
"#,
    );
}

#[test]
fn sized_constant_value_positive_overflow() {
    assert_js!(
        r#"
fn go() {
  <<80_000:16>>
}
"#,
    );
}

#[test]
fn sized_constant_value_negative_overflow() {
    assert_js!(
        r#"
fn go() {
  <<-80_000:16>>
}
"#,
    );
}

#[test]
fn sized_constant_value_max_size_for_compile_time_evaluation() {
    assert_js!(
        r#"
fn go() {
  <<-1:48>>
}
"#,
    );
}

#[test]
fn sized_big_endian_constant_value() {
    assert_js!(
        r#"
fn go() {
  <<256:16-big>>
}
"#,
    );
}

#[test]
fn sized_big_endian_dynamic_value() {
    assert_js!(
        r#"
fn go(i: Int) {
  <<i:16-big>>
}
"#,
    );
}

#[test]
fn sized_little_endian_constant_value() {
    assert_js!(
        r#"
fn go() {
  <<256:16-little>>
}
"#,
    );
}

#[test]
fn sized_little_endian_dynamic_value() {
    assert_js!(
        r#"
fn go(i: Int) {
  <<i:16-little>>
}
"#,
    );
}

#[test]
fn explicit_sized_constant_value() {
    assert_js!(
        r#"
fn go() {
  <<256:size(32)>>
}
"#,
    );
}

#[test]
fn explicit_sized_dynamic_value() {
    assert_js!(
        r#"
fn go(i: Int) {
  <<i:size(32)>>
}
"#,
    );
}

#[test]
fn variable_sized() {
    assert_js!(
        r#"
fn go(x, y) {
  <<x:size(y)>>
}
"#,
    );
}

#[test]
fn variable() {
    assert_js!(
        r#"
fn go(x) {
  <<256, 4, x>>
}
"#,
    );
}

#[test]
fn utf8() {
    assert_js!(
        r#"
fn go(x) {
  <<256, 4, x, "Gleam":utf8>>
}
"#,
    );
}

#[test]
fn match_utf8_with_escape_chars() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<"\"\\\r\n\t\f\u{1f600}">> = x
}
"#,
    );
}

#[test]
fn match_utf8() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<"Gleam 👍":utf8>> = x
}
"#,
    );
}

#[test]
fn utf8_codepoint() {
    assert_js!(
        r#"
fn go(x) {
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
fn go(x) {
  <<x:bits>>
}
"#,
    );
}

#[test]
fn bits() {
    assert_js!(
        r#"
fn go(x) {
  <<x:bits>>
}
"#,
    );
}

#[test]
fn bit_array_sliced() {
    assert_js!(
        r#"
fn go(x) {
  <<<<0xAB>>:bits-4>>
}
"#,
    );
}

#[test]
fn bit_array_dynamic_slice() {
    assert_js!(
        r#"
fn go(x) {
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
fn go(x) {
  let assert <<>> = x
}
"#,
    );
}

#[test]
fn match_bytes() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<1, y>> = x
}
"#,
    );
}

#[test]
fn match_sized() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:16, b:8>> = x
}
"#,
    );
}

#[test]
fn match_sized_unaligned() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:17, b:7>> = x
}
"#,
    );
}

#[test]
fn match_sized_constant_pattern() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<1234:16, 123:8>> = x
}
"#,
    );
}

#[test]
fn match_unsigned() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:unsigned>> = x
}
"#,
    );
}

#[test]
fn match_unsigned_constant_pattern() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<-2:unsigned>> = x
}
"#,
    );
}

#[test]
fn match_signed() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:signed>> = x
}
"#,
    );
}

#[test]
fn match_signed_constant_pattern() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<-1:signed>> = x
}
"#,
    );
}

#[test]
fn match_sized_big_endian() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:16-big>> = x
}
"#,
    );
}

#[test]
fn match_sized_big_endian_constant_pattern() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<1234:16-big>> = x
}
"#,
    );
}

#[test]
fn match_sized_little_endian() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:16-little>> = x
}
"#,
    );
}

#[test]
fn match_sized_little_endian_constant_pattern() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<1234:16-little>> = x
}
"#,
    );
}

#[test]
fn match_sized_big_endian_unsigned() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:16-big-unsigned>> = x
}
"#,
    );
}

#[test]
fn match_sized_big_endian_unsigned_constant_pattern() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<1234:16-big-unsigned>> = x
}
"#,
    );
}

#[test]
fn match_sized_big_endian_signed() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:16-big-signed>> = x
}
"#,
    );
}

#[test]
fn match_sized_big_endian_signed_constant_pattern() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<1234:16-big-signed>> = x
}
"#,
    );
}

#[test]
fn match_sized_little_endian_unsigned() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:16-little-unsigned>> = x
}
"#,
    );
}

#[test]
fn match_sized_little_endian_unsigned_constant_pattern() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<1234:16-little-unsigned>> = x
}
"#,
    );
}

#[test]
fn match_sized_little_endian_signed() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:16-little-signed>> = x
}
"#,
    );
}

#[test]
fn match_sized_little_endian_signed_constant_pattern() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<1234:16-little-signed>> = x
}
"#,
    );
}

#[test]
fn match_dynamic_size() {
    assert_js!(
        r#"
fn go(x) {
  let n = 16
  let assert <<a:size(n)>> = x
}
"#
    );
}

#[test]
fn match_dynamic_size_with_other_segments() {
    assert_js!(
        r#"
fn go(x) {
  let n = 16
  let m = 32
  let assert <<first:size(8), a:size(n), b:size(m), rest:bits>> = x
}
"#
    );
}

#[test]
fn match_dynamic_bits_size_error() {
    assert_js_error!(
        r#"
fn go(x) {
  let n = 16
  let assert <<a:bits-size(n)>> = x
}
"#
    );
}

#[test]
fn discard_sized() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<_:16, _:8>> = x
  let assert <<_:16-little-signed, _:8>> = x
}
"#,
    );
}

#[test]
fn match_sized_value() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<i:16>> = x
}
"#,
    );
}

#[test]
fn match_sized_value_constant_pattern() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<258:16>> = x
}
"#,
    );
}

#[test]
fn match_float() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:float, b:int>> = x
}
"#,
    );
}

#[test]
fn match_float_big_endian() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:float-big, b:int>> = x
}
"#,
    );
}

#[test]
fn match_float_little_endian() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:float-little, b:int>> = x
}
"#,
    );
}

#[test]
fn match_float_sized() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:float-32, b:int>> = x
}
"#,
    );
}

#[test]
fn match_float_sized_big_endian() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:float-32-big, b:int>> = x
}
"#,
    );
}

#[test]
fn match_float_sized_little_endian() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:float-32-little, b:int>> = x
}
"#,
    );
}

#[test]
fn match_float_16_bit_error() {
    assert_js_error!(
        r#"
fn go(x) {
  let assert <<a:float-size(16)>> = x
}
"#
    );
}

#[test]
fn match_rest() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<_, b:bytes>> = <<1,2,3>>
}
"#,
    );
}

#[test]
fn match_bytes_with_size() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<f:bytes-2>> = <<1, 2>>
}
"#,
    );
}

#[test]
fn match_bits_with_size() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<_:4, f:bits-2, _:1>> = <<0x77:7>>
}
"#,
    );
}

#[test]
fn match_rest_bytes() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<_, b:bytes>> = <<1,2,3>>
}
"#,
    );
}

#[test]
fn match_rest_bits() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<_, b:bits>> = <<1,2,3>>
}
"#,
    );
}

#[test]
fn match_rest_bits_unaligned() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<_:5, b:bits>> = <<1,2,3>>
}
"#,
    );
}

#[test]
fn match_binary_size() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<_, a:2-bytes>> = x
  let assert <<_, b:bytes-size(2)>> = x
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
  let assert <<a:bits>> = <<>>
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
fn negative_size() {
    assert_js!(
        r#"
fn go(x: Int) {
  <<x:size(-1)>>
}
"#,
    );
}

#[test]
fn negative_size_constant_value() {
    assert_js!(
        r#"
fn go(x: Int) {
  <<1:size(-1)>>
}
"#,
    );
}

#[test]
fn bit_array_literal_string_constant_is_treated_as_utf8() {
    assert_js!(r#"const a = <<"hello", " ", "world">>"#);
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
