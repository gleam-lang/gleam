pub fn remainder_1_test() {
  assert 1 % 1 == 0
}

pub fn remainder_2_test() {
  assert 1 % 0 == 0
}

pub fn remainder_3_test() {
  assert 3 % 2 == 1
}

pub fn remainder_4_test() {
  assert 3 % 0 == 0
}

pub fn remainder_5_test() {
  assert 3 % -2 == 1
}

pub fn remainder_6_test() {
  assert 3 % -0 == 0
}

pub fn remainder_7_test() {
  assert -13 % 3 == -1
}

pub fn remainder_8_test() {
  assert 13 % -3 == 1
}

pub fn remainder_9_test() {
  assert -13 % -3 == -1
}
