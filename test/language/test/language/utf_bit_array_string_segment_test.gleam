pub fn literal_utf8_string_segment_test() {
  let bit_array = <<"abc":utf8>>
  assert bit_array == <<97, 98, 99>>
}

pub fn variable_utf8_string_segment_test() {
  let string = "abc"
  let bit_array = <<string:utf8>>
  assert bit_array == <<97, 98, 99>>
}

pub fn literal_utf16_little_string_segment_test() {
  let bit_array = <<"abc":utf16-little>>
  assert bit_array == <<97, 0, 98, 0, 99, 0>>
}

pub fn variable_utf16_little_string_segment_test() {
  let string = "abc"
  let bit_array = <<string:utf16-little>>
  assert bit_array == <<97, 0, 98, 0, 99, 0>>
}

pub fn literal_utf16_big_string_segment_test() {
  let bit_array = <<"abc":utf16-big>>
  assert bit_array == <<0, 97, 0, 98, 0, 99>>
}

pub fn variable_utf16_big_string_segment_test() {
  let string = "abc"
  let bit_array = <<string:utf16-big>>
  assert bit_array == <<0, 97, 0, 98, 0, 99>>
}

pub fn literal_utf32_little_string_segment_test() {
  let bit_array = <<"abc":utf32-little>>
  assert bit_array == <<97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0>>
}

pub fn variable_utf32_little_string_segment_test() {
  let string = "abc"
  let bit_array = <<string:utf32-little>>
  assert bit_array == <<97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0>>
}

pub fn literal_utf32_big_string_segment_test() {
  let bit_array = <<"abc":utf32-big>>
  assert bit_array == <<0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99>>
}

pub fn variable_utf32_big_string_segment_test() {
  let string = "abc"
  let bit_array = <<string:utf32-big>>
  assert bit_array == <<0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99>>
}
