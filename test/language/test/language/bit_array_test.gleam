import ffi

pub fn utf8_emoji_equal_test() {
  let left = <<"Gleam":utf8, "👍":utf8>>
  let right = <<"Gleam":utf8, "👍":utf8>>
  assert left == right
}

pub fn utf8_emoji_not_equal_test() {
  let left = <<"Gleam":utf8, "👍":utf8>>
  let right = <<"👍":utf8>>
  assert left != right
}

pub fn utf8_ascii_test() {
  let left = <<"abc":utf8>>
  let right = <<97, 98, 99>>
  assert left == right
}

pub fn utf8_unicode_escape_test() {
  let left = <<"😀":utf8>>
  let right = <<"\u{1F600}":utf8>>
  assert left == right
}

pub fn nested_bit_array_test() {
  let left = <<<<1>>:bits, 2>>
  let right = <<1, 2>>
  assert left == right
}

pub fn int_segment_test() {
  let left = <<1>>
  let right = <<1:int>>
  assert left == right
}

pub fn int_16_test() {
  let left = <<80_000:16>>
  let right = <<56, 128>>
  assert left == right
}

pub fn negative_int_16_test() {
  let left = <<-80_000:16>>
  let right = <<199, 128>>
  assert left == right
}

pub fn negative_int_64_test() {
  let left = <<-1:64>>
  let right = <<255, 255, 255, 255, 255, 255, 255, 255>>
  assert left == right
}

pub fn negative_int_56_test() {
  let left = <<-489_391_639_457_909_760:56>>
  let right = <<53, 84, 229, 150, 16, 180, 0>>
  assert left == right
}

pub fn float_16_zero_test() {
  let left = <<0, 0>>
  let right = <<0.0:float-16>>
  assert left == right
}

pub fn float_16_little_negative_test() {
  let left = <<0x00, 0xbc>>
  let right = <<-1.0:float-16-little>>
  assert left == right
}

pub fn float_16_little_test() {
  let left = <<0xf0, 0x3c>>
  let right = <<1.234375:float-16-little>>
  assert left == right
}

pub fn float_16_negative_large_test() {
  let left = <<0xfb, 0xff>>
  let right = <<-65_504.0:float-16>>
  assert left == right
}

pub fn float_64_test() {
  let left = <<63, 240, 0, 0, 0, 0, 0, 0>>
  let right = <<1.0:float>>
  assert left == right
}

pub fn float_32_test() {
  let left = <<63, 128, 0, 0>>
  let right = <<1.0:float-32>>
  assert left == right
}

pub fn float_64_little_test() {
  let left = <<0, 0, 0, 0, 0, 0, 240, 63>>
  let right = <<1.0:float-64-little>>
  assert left == right
}

pub fn float_64_big_test() {
  let left = <<63, 240, 0, 0, 0, 0, 0, 0>>
  let right = <<1.0:float-64-big>>
  assert left == right
}

pub fn pattern_match_utf8_test() {
  let result = case <<0x20, "😀👍":utf8, 0x20>> {
    <<" ":utf8, "😀👍":utf8, 0x20>> -> True
    _ -> False
  }
  assert result == True
}

pub fn pattern_match_bytes_sliced_test() {
  let assert <<_, b:bytes-3, _>> = <<1, 2, 3, 4, 5>>
  let assert <<_, rest:bytes>> = b
  assert rest == <<3, 4>>
}

pub fn matching_zero_length_segment_test() {
  let size = 0
  let data = <<>>
  let result = case data {
    <<_:bytes-size(size), _:bytes>> -> "ok"
    _ -> "this cause should not be reached"
  }
  assert result == "ok"
}

pub fn float_16_erlang_test() {
  let left = <<60, 0>>
  let right = <<1.0:float-16>>
  assert left == right
}

// https://github.com/gleam-lang/gleam/issues/3375
pub fn assignment_int_pattern_test() {
  let assert <<10 as a, _>> = <<10, 20>>
  assert a == 10
}

pub fn assignment_float_pattern_test() {
  let assert <<3.14 as pi:float>> = <<3.14>>
  assert pi == 3.14
}

pub fn assignment_string_pattern_test() {
  let assert <<"Hello" as h:utf8, ", world!">> = <<"Hello, world!">>
  assert h == "Hello"
}

@target(erlang)
pub fn pattern_match_utf16_codepoint_little_test() {
  let assert <<codepoint:utf16_codepoint-little>> = <<"🌍":utf16-little>>
  assert codepoint == ffi.utf_codepoint(127_757)
}

@target(erlang)
pub fn pattern_match_utf32_codepoint_little_test() {
  let assert <<codepoint:utf32_codepoint-little>> = <<"🌍":utf32-little>>
  assert codepoint == ffi.utf_codepoint(127_757)
}

pub fn unicode_overflow_test() {
  // In erlang, literally creating binaries can cause entries to overflow.
  // For example `<<"🌵">> == <<"5">>` evaluates to true.
  // This checks that we are not doing that.
  // See: https://github.com/gleam-lang/gleam/issues/457
  let string = "5"
  assert "🌵" != string
}
