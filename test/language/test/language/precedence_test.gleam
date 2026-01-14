pub fn precedence_1_test() {
  assert 7 == 1 + 2 * 3
}

pub fn precedence_2_test() {
  assert 5 == 3 * 1 + 2
}

pub fn precedence_3_test() {
  assert 9 == { 1 + 2 } * 3
}

pub fn precedence_4_test() {
  assert 9 == 3 * { 1 + 2 }
}

pub fn precedence_5_test() {
  assert 11 == 1 + 2 * 3 + 4
}

pub fn precedence_6_test() {
  assert 26 == 2 * 3 + 4 * 5
}

pub fn precedence_7_test() {
  assert 4 == 2 * { 3 + 1 } / 2
}

pub fn precedence_8_test() {
  assert -17 == 5 + 3 / 3 * 2 - 6 * 4
}

pub fn precedence_9_test() {
  assert -31 == -5 + -3 / -3 * -2 - -6 * -4
}

pub fn precedence_10_test() {
  let a = 5
  let b = 3
  let c = 3
  let d = 2
  let e = 6
  let f = 4
  assert -17 == a + b / c * d - e * f
}

pub fn precedence_11_test() {
  let a = 5
  let b = 3
  let c = 3
  let d = 2
  let e = 6
  let f = 4
  assert -31 == -a + -b / -c * -d - -e * -f
}
