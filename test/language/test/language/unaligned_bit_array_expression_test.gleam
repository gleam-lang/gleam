pub fn unaligned_1_test() {
  let left = <<0xFF:6, 0:2>>
  let right = <<0xFC>>
  assert left == right
}

pub fn unaligned_2_test() {
  let left = <<0xFF:6, 0:3, 0x75:9>>
  let right = <<252, 29, 1:2>>
  assert left == right
}

pub fn unaligned_3_test() {
  let left = <<-1:55, 44:11-little, 0x75:9-big>>
  let right = <<255, 255, 255, 255, 255, 255, 254, 88, 14, 5:3>>
  assert left == right
}

pub fn unaligned_4_test() {
  let left = <<0:1, 2:2, 2:3, 1:1>>
  let right = <<0b0100101:7>>
  assert left == right
}

pub fn unaligned_5_test() {
  let left = <<-100:6, -10:32-little, -10:32-big, -100:48-big, -100:48-little>>
  let right = <<
    115, 219, 255, 255, 255, 255, 255, 255, 219, 255, 255, 255, 255, 254, 114,
    115, 255, 255, 255, 255, 63:6,
  >>
  assert left == right
}

pub fn unaligned_6_test() {
  let left = <<2:3, 2.9283123:float-little, -1.375e5:32-float-big>>
  let right = <<91, 153, 120, 255, 229, 205, 160, 232, 25, 0, 200, 224, 0:3>>
  assert left == right
}

pub fn unaligned_7_test() {
  let left = <<
    7:6,
    <<1:3>>:bits,
    <<1, 2, 3>>:bits,
    1:1,
    <<-1124.789e4:float-little>>:bits,
  >>
  let right = <<28, 128, 129, 1, 192, 0, 0, 16, 8, 157, 25, 112, 1:2>>
  assert left == right
}

pub fn unaligned_8_test() {
  let left = <<9_444_732_965_739_289_353_650_176:75>>
  let right = <<255, 255, 255, 255, 255, 248, 0, 0, 0, 0:size(3)>>
  assert left == right
}

pub fn unaligned_9_test() {
  let left = <<0b100101:6>>
  let right = <<<<0b10010100>>:bits-6>>
  assert left == right
}

pub fn unaligned_10_test() {
  let left = <<0xE7>>
  let right = <<<<0xEC>>:bits-4, 7:4>>
  assert left == right
}

pub fn unaligned_11_test() {
  let three = 3
  let two = 2
  let left = <<0b11001:5>>
  let right = <<<<0b11011100>>:bits-size(three), 1:size(two)>>
  assert left == right
}
