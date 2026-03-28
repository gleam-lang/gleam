pub fn dynamic_size_1_test() {
  let size = 8
  let assert <<value:size(size)>> = <<42>>
  assert value == 42
}

// Arithmetic operators in bit-array size expressions must be left-associative.
// These tests guard against regressions of the parser bug fixed in #5456.

pub fn size_sub_is_left_associative_test() {
  let a = 10
  let b = 3
  let c = 2
  // (a - b) - c = 5
  let assert <<value:bytes-size(a - b - c)>> = <<1, 2, 3, 4, 5>>
  assert value == <<1, 2, 3, 4, 5>>
}

pub fn size_mul_in_pattern_test() {
  let a = 1
  let b = 2
  let c = 3
  // (a * b) * c = 6. Multiplication is mathematically associative so both
  // left- and right-associative parsers produce the same result; this test
  // guards that chained * works correctly in pattern sizes.
  let assert <<value:bytes-size(a * b * c)>> = <<1, 2, 3, 4, 5, 6>>
  assert value == <<1, 2, 3, 4, 5, 6>>
}

pub fn size_sub_add_chain_is_left_associative_test() {
  let a = 10
  let b = 3
  let c = 5
  // (a - b) + c = 12, but a - (b + c) = 2
  let assert <<value:bytes-size(a - b + c)>> = <<
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
  >>
  assert value == <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12>>
}

pub fn size_mul_binds_tighter_than_add_test() {
  let a = 2
  let b = 2
  let c = 3
  // a + (b * c) = 8
  let assert <<value:bytes-size(a + b * c)>> = <<1, 2, 3, 4, 5, 6, 7, 8>>
  assert value == <<1, 2, 3, 4, 5, 6, 7, 8>>
}

pub fn dynamic_size_2_test() {
  let size = 3
  let other_size = 5
  let third_size = 8
  let assert <<
    value:size(size),
    second:size(other_size),
    last:size(third_size),
  >> = <<5:3, 8:5, 128>>
  assert #(value, second, last) == #(5, 8, 128)
}

pub fn dynamic_size_3_test() {
  let size = 2
  let assert <<first_bytes:bytes-size(size), _rest:bytes>> = <<1, 2, 3, 4>>
  assert first_bytes == <<1, 2>>
}

pub fn dynamic_size_4_test() {
  let size = 6
  let assert <<bits:bits-size(size), _rest:bits>> = <<0b10010100, 6, 2938>>
  assert bits == <<0b100101:6>>
}

pub fn dynamic_size_5_test() {
  let size = 7
  let assert <<123:size(size)>> = <<123:7>>
}

pub fn dynamic_size_6_test() {
  let size = 4
  let size = size + 2
  let assert <<value:size(size)>> = <<61:6>>
  assert value == 61
}
