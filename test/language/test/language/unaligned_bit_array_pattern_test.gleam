pub fn unaligned_1_test() {
  let assert <<a:4, b:4, c:3, d:4, e:1>> = <<0xAB, 0b11010101>>
  assert #(a, b, c, d, e) == #(0xA, 0xB, 0b110, 0b1010, 0b1)
}

pub fn unaligned_2_test() {
  let assert <<a:12, b:4>> = <<0xB6, 0xE3>>
  assert #(a, b) == #(0xB6E, 0x03)
}

pub fn unaligned_3_test() {
  let assert <<a:4, b:12>> = <<0xB6, 0xE3>>
  assert #(a, b) == #(0x0B, 0x6E3)
}

pub fn unaligned_4_test() {
  let assert <<a:12-little, b:4>> = <<0xB6, 0xE3>>
  assert #(a, b) == #(0xEB6, 0x03)
}

pub fn unaligned_5_test() {
  let assert <<a:4, b:12-little>> = <<0xB6, 0xE3>>
  assert #(a, b) == #(0x0B, 0x36E)
}

pub fn unaligned_6_test() {
  let assert <<a:5, b:11>> = <<0xB6, 0xE3>>
  assert #(a, b) == #(22, 1763)
}

pub fn unaligned_7_test() {
  let assert <<_:8, a:17>> = <<0xFF, 0xB6, 0xE3, 1:1>>
  assert a == 93_639
}

pub fn unaligned_8_test() {
  let assert <<a:17, _:bits>> = <<0xA6, 0xE3, 6:3>>
  assert a == 85_447
}

pub fn unaligned_9_test() {
  let assert <<a:17, _:bits>> = <<0xA6, 0xE3, 6:3>>
  assert a == 85_447
}

pub fn unaligned_10_test() {
  let assert <<a:7-signed, _:bits>> = <<0b10011010>>
  assert a == -51
}

pub fn unaligned_11_test() {
  let assert <<_:5, a:13-big, _:bits>> = <<0xA5, 0x6C, 0xAA>>
  assert a == 5554
}

pub fn unaligned_12_test() {
  let assert <<_:5, a:13-little-signed, _:bits>> = <<0xA5, 0x6C, 0xAA>>
  assert a == -3411
}

pub fn unaligned_13_test() {
  let assert <<_:3, a:8-big-signed, _:bits>> = <<253, 94>>
  assert a == -22
}

pub fn unaligned_14_test() {
  let assert <<_:3, a:12-big-unsigned, _:bits>> = <<233, 164>>
  assert a == 1234
}

pub fn unaligned_15_test() {
  let assert <<_:3, a:12-little, _:bits>> = <<250, 72>>
  assert a == 1234
}

pub fn unaligned_16_test() {
  let assert <<_:7, a:22, _:bits>> = <<250, 72, 223, 189>>
  assert a == 596_983
}

pub fn unaligned_17_test() {
  let assert <<_:1, a:23, _:bits>> = <<146, 192, 70, 25, 1:1>>
  assert a == 1_228_870
}

pub fn unaligned_18_test() {
  let assert <<_:1, a:32>> = <<217, 150, 209, 191, 0:1>>
  assert a == 3_006_112_638
}

pub fn unaligned_19_test() {
  let assert <<_:1, a:32-signed>> = <<146, 192, 70, 25, 1:1>>
  assert a == 629_181_491
}

pub fn unaligned_20_test() {
  let assert <<_:1, a:32-little-unsigned>> = <<251, 24, 47, 227, 1:1>>
  assert a == 3_344_904_438
}

pub fn unaligned_21_test() {
  let assert <<a:33-little-unsigned>> = <<240, 102, 91, 101, 1:1>>
  assert a == 5_995_456_240
}

pub fn unaligned_22_test() {
  let assert <<a:40-big-signed, _:8>> = <<231, 255, 255, 255, 254, 123>>
  assert a == -103_079_215_106
}

pub fn unaligned_23_test() {
  let assert <<_:1, a:54-big-unsigned, _:bits>> = <<
    0, 231, 255, 255, 253, 123, 17,
  >>
  assert a == 127_543_348_739_464
}

pub fn unaligned_24_test() {
  let assert <<_:8, a:54-little-signed, _:bits>> = <<
    142, 231, 255, 255, 253, 123, 17, 139,
  >>
  assert a == -8_425_025_061_257_241
}

pub fn unaligned_25_test() {
  let assert <<_:7, a:55-little-signed, _:bits>> = <<
    142, 231, 255, 255, 253, 123, 17, 139,
  >>
  assert a == -8_293_899_692_933_261
}

pub fn unaligned_26_test() {
  let assert <<_:8, a:40-big-signed, _:8>> = <<
    142,
    231,
    255,
    255,
    253,
    123,
    17,
  >>
  assert a == -103_079_215_749
}

pub fn unaligned_27_test() {
  let assert <<_:14, a:71-little-signed, _:bits>> = <<
    250, 72, 223, 189, 41, 97, 165, 0, 0, 0, 0, 177,
  >>
  assert a == 70_821_197_049_655
}

pub fn unaligned_28_test() {
  let assert <<_:14, a:71-big-signed, _:bits>> = <<
    250, 72, 223, 189, 41, 97, 165, 0, 0, 0, 0, 177,
  >>
  assert a == 515_906_807_693_217_628_160
}

pub fn unaligned_29_test() {
  let assert <<_:11, a:float-32, _:bits>> = <<112, 152, 127, 244, 0, 7, 0:2>>
  assert a == -511.25
}

pub fn unaligned_30_test() {
  let assert <<_:7, a:float-little, _:bits>> = <<
    8, 0, 0, 0, 1, 129, 39, 103, 129, 127:7,
  >>
  assert a == -5011.75
}

pub fn unaligned_31_test() {
  let assert <<a:bits-7, b:bits-3>> = <<0b11001011, 0b01:2>>
  assert #(a, b) == #(<<0b1100101:7>>, <<0b101:3>>)
}

pub fn unaligned_32_test() {
  let assert <<a:bits-16, b:bits-13, c:bits-11, d:bits-24>> = <<
    0x47, 0x9A, 0x25, 0x0C, 0xDA, 0xF1, 0xEE, 0x31,
  >>
  assert #(a, b, c, d)
    == #(<<71, 154>>, <<37, 1:5>>, <<155, 2:3>>, <<0xF1, 0xEE, 0x31>>)
}

pub fn unaligned_33_test() {
  let assert <<a:bits-3, b:bytes-2, c:bytes>> = <<
    0b110:3, 0x12, 0xAB, 0x95, 0xFE,
  >>
  assert #(a, b, c) == #(<<0b110:3>>, <<0x12, 0xAB>>, <<0x95, 0xFE>>)
}

pub fn unaligned_34_test() {
  let result = case <<0x12, 0xAB, 0x95, 0xFE>> {
    <<0x34, _:5, _:bytes>> -> True
    _ -> False
  }
  assert result == False
}
