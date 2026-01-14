pub fn dynamic_size_1_test() {
  let size = 8
  let assert <<value:size(size)>> = <<42>>
  assert value == 42
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
