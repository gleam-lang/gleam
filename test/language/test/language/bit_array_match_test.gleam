import importable

pub fn match_1_test() {
  let assert <<1, x>> = <<1, 2>>
  assert x == 2
}

pub fn match_2_test() {
  let assert <<a:8>> = <<1>>
  assert a == 1
}

pub fn match_3_test() {
  let assert <<a:16, b:8>> = <<1, 2, 3>>
  assert #(a, b) == #(258, 3)
}

pub fn match_4_test() {
  let assert <<a:int-32-little-signed, b:signed-big-24>> = <<
    255, 255, 255, 255, 255, 216, 240,
  >>
  assert #(a, b) == #(-1, -10_000)
}

pub fn match_5_test() {
  let assert <<a:16-unsigned, b:40-signed-little>> = <<
    255, 255, 255, 255, 240, 216, 255,
  >>
  assert #(a, b) == #(65_535, -655_294_465)
}

pub fn match_6_test() {
  let assert <<a:64-signed>> = <<255, 255, 255, 255, 255, 255, 255, 255>>
  assert a == -1
}

pub fn match_7_test() {
  let assert <<a:56-big-unsigned>> = <<0x00, 0xaa, 255, 255, 255, 255, 255>>
  assert a == 0xaaffffffffff
}

pub fn match_8_test() {
  let assert <<a:56-little-unsigned>> = <<255, 255, 255, 255, 255, 0xaa, 0x00>>
  assert a == 0xaaffffffffff
}

pub fn match_9_test() {
  let assert <<a:float, b:int>> = <<63, 240, 0, 0, 0, 0, 0, 0, 1>>
  assert #(a, b) == #(1.0, 1)
}

pub fn match_10_test() {
  let assert <<a:float>> = <<1.23:float>>
  assert a == 1.23
}

pub fn match_11_test() {
  let assert <<a:float-32>> = <<63, 176, 0, 0>>
  assert a == 1.375
}

pub fn match_12_test() {
  let assert <<a:64-float-little>> = <<61, 10, 215, 163, 112, 61, 18, 64>>
  assert a == 4.56
}

pub fn match_13_test() {
  let assert <<a:float-16>> = <<0, 0>>
  assert a == 0.0
}

pub fn match_14_test() {
  let assert <<a:float-16>> = <<0x3C, 0xF0>>
  assert a == 1.234375
}

pub fn match_15_test() {
  let assert <<a:float-16-little>> = <<0xFF, 0xFB>>
  assert a == -65_504.0
}

pub fn match_16_test() {
  let assert <<_, rest:bytes>> = <<1>>
  assert rest == <<>>
}

pub fn match_17_test() {
  let assert <<_, rest:bytes>> = <<1, 2, 3>>
  assert rest == <<2, 3>>
}

pub fn match_18_test() {
  let assert <<x:2-bytes, _:bytes>> = <<1, 2, 3>>
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
  assert <<71, 108, 101, 97, 109>> == <<"Gleam":utf8>>
}

pub fn match_22_test() {
  let assert <<i:40-signed, _:bits>> = <<231, 255, 255, 255, 254, 123>>
  assert i == -103_079_215_106
}

pub fn match_23_test() {
  let assert <<_, i:40-signed, _:bits>> = <<142, 231, 255, 255, 253, 123, 17>>
  assert i == -103_079_215_749
}

// https://github.com/gleam-lang/gleam/issues/4712
pub fn multiple_variable_segments_test() {
  let assert <<a, b:size(a), c:size(b)>> = <<2, 3:2, 7:3>>
  assert a + b + c == 12
}
