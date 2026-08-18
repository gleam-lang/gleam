// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import importable

pub fn match_1_test() {
  let x = <<1, 2>>
  let assert <<1, x>> = x
  assert x == 2
}

pub fn match_2_test() {
  let x = <<1>>
  let assert <<a:8>> = x
  assert a == 1
}

pub fn match_3_test() {
  let x = <<1, 2, 3>>
  let assert <<a:16, b:8>> = x
  assert #(a, b) == #(258, 3)
}

pub fn match_4_test() {
  let x = <<255, 255, 255, 255, 255, 216, 240>>
  let assert <<a:int-32-little-signed, b:signed-big-24>> = x
  assert #(a, b) == #(-1, -10_000)
}

pub fn match_5_test() {
  let x = <<255, 255, 255, 255, 240, 216, 255>>
  let assert <<a:16-unsigned, b:40-signed-little>> = x
  assert #(a, b) == #(65_535, -655_294_465)
}

pub fn match_6_test() {
  let x = <<255, 255, 255, 255, 255, 255, 255, 255>>
  let assert <<a:64-signed>> = x
  assert a == -1
}

pub fn match_7_test() {
  let x = <<0x00, 0xaa, 255, 255, 255, 255, 255>>
  let assert <<a:56-big-unsigned>> = x
  assert a == 0xaaffffffffff
}

pub fn match_8_test() {
  let x = <<255, 255, 255, 255, 255, 0xaa, 0x00>>
  let assert <<a:56-little-unsigned>> = x
  assert a == 0xaaffffffffff
}

pub fn match_9_test() {
  let x = <<63, 240, 0, 0, 0, 0, 0, 0, 1>>
  let assert <<a:float, b:int>> = x
  assert #(a, b) == #(1.0, 1)
}

pub fn match_10_test() {
  let x = <<1.23:float>>
  let assert <<a:float>> = x
  assert a == 1.23
}

pub fn match_11_test() {
  let x = <<63, 176, 0, 0>>
  let assert <<a:float-32>> = x
  assert a == 1.375
}

pub fn match_12_test() {
  let x = <<61, 10, 215, 163, 112, 61, 18, 64>>
  let assert <<a:64-float-little>> = x
  assert a == 4.56
}

pub fn match_13_test() {
  let x = <<0, 0>>
  let assert <<a:float-16>> = x
  assert a == 0.0
}

pub fn match_14_test() {
  let x = <<0x3C, 0xF0>>
  let assert <<a:float-16>> = x
  assert a == 1.234375
}

pub fn match_15_test() {
  let x = <<0xFF, 0xFB>>
  let assert <<a:float-16-little>> = x
  assert a == -65_504.0
}

pub fn match_16_test() {
  let x = <<1>>
  let assert <<_, rest:bytes>> = x
  assert rest == <<>>
}

pub fn match_17_test() {
  let x = <<1, 2, 3>>
  let assert <<_, rest:bytes>> = x
  assert rest == <<2, 3>>
}

pub fn match_18_test() {
  let x = <<1, 2, 3>>
  let assert <<x:2-bytes, _:bytes>> = x
  assert x == <<1, 2>>
}

pub fn match_19_test() {
  assert <<
      0x1,
      2,
      2:size(16),
      0x4:size(32),
      "Gleam":utf8,
      4.2:float,
      <<<<1, 2, 3>>:bits, "Gleam":utf8, 1024>>:bits,
    >>
    == importable.get_bit_array()
}

pub fn match_20_test() {
  assert <<
      0x1,
      2,
      2:size(16),
      0x4:size(32),
      "Gleam":utf8,
      4.2:float,
      <<<<1, 2, 3>>:bits, "Gleam":utf8, 1024>>:bits,
    >>
    == importable.data
}

pub fn match_21_test() {
  let x = <<"Gleam":utf8>>
  assert <<71, 108, 101, 97, 109>> == x
}

pub fn match_22_test() {
  let x = <<231, 255, 255, 255, 254, 123>>
  let assert <<i:40-signed, _:bits>> = x
  assert i == -103_079_215_106
}

pub fn match_23_test() {
  let x = <<142, 231, 255, 255, 253, 123, 17>>
  let assert <<_, i:40-signed, _:bits>> = x
  assert i == -103_079_215_749
}

// https://github.com/gleam-lang/gleam/issues/4712
pub fn multiple_variable_segments_test() {
  let x = <<2, 3:2, 7:3>>
  let assert <<a, b:size(a), c:size(b)>> = x
  assert a + b + c == 12
}

// https://github.com/gleam-lang/gleam/issues/5208
pub fn unit_ignores_bytes_option_test() {
  let x = <<1:6>>
  let assert <<x:unit(2)-bytes-size(3)>> = x
  assert x == <<1:6>>
}

pub fn unit_ignores_bytes_option_regardless_of_order_test() {
  let x = <<1:6>>
  let assert <<x:bytes-unit(2)-size(3)>> = x
  assert x == <<1:6>>
}

// https://github.com/gleam-lang/gleam/issues/6182
pub fn match_on_empty_segment_test() {
  let x = <<>>
  let assert <<"":utf8>> = x
  Nil
}

// https://github.com/gleam-lang/gleam/issues/6182
pub fn match_on_empty_segment_in_middle_test() {
  let x = <<1, 2>>
  let assert <<1, "":utf8, 2>> = x
  Nil
}

// https://github.com/gleam-lang/gleam/issues/6182
pub fn match_on_empty_string_with_alias_test() {
  let x = <<1, 2>>
  let assert <<1, "" as a:utf8, 2>> = x
  assert a == ""
  Nil
}

pub fn bit_array_slice_assignment_inside_a_nested_pattern_after_a_guard_test() {
  let go = fn(b: Bool, flag: Bool, r: Result(BitArray, Nil)) -> BitArray {
    case b, r {
      True, _ if flag -> <<1>>
      _, Ok(<<x:bits>>) -> x
      _, _ -> <<>>
    }
  }

  assert go(True, True, Ok(<<2>>)) == <<1>>
  assert go(True, False, Ok(<<2>>)) == <<2>>
  assert go(False, True, Ok(<<2>>)) == <<2>>
  assert go(False, True, Error(Nil)) == <<>>
}
