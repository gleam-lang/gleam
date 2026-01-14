pub fn addition_1_test() {
  assert 0 + 0 == 0
}

pub fn addition_2_test() {
  assert 1 + 1 == 2
}

pub fn addition_3_test() {
  assert 5 + 1 == 6
}

pub fn addition_4_test() {
  assert 1 + 3 == 4
}

pub fn addition_5_test() {
  assert 1 + -3 == -2
}

pub fn subtraction_1_test() {
  assert 0 - 0 == 0
}

pub fn subtraction_2_test() {
  assert 1 - 1 == 0
}

pub fn subtraction_3_test() {
  assert 5 - 1 == 4
}

pub fn subtraction_4_test() {
  assert 1 - 3 == -2
}

pub fn subtraction_5_test() {
  assert 1 - -3 == 4
}

pub fn multiplication_1_test() {
  assert 0 * 0 == 0
}

pub fn multiplication_2_test() {
  assert 1 * 1 == 1
}

pub fn multiplication_3_test() {
  assert 2 * 2 == 4
}

pub fn multiplication_4_test() {
  assert 2 * 4 == 8
}

pub fn multiplication_5_test() {
  assert -2 * 4 == -8
}

pub fn multiplication_6_test() {
  assert 2 * -4 == -8
}

pub fn precedence_1_test() {
  assert 2 * 2 + 3 == 7
}

pub fn precedence_2_test() {
  assert 2 + 2 * 3 == 8
}

pub fn precedence_3_test() {
  assert 2 * { 2 + 3 } == 10
}

pub fn precedence_4_test() {
  assert { 2 + 2 } * 3 == 12
}

pub fn hex_int_test() {
  let int = 15
  assert 0xF == int
}

pub fn octal_int_test() {
  let int = 15
  assert 0o17 == int
}

pub fn binary_int_test() {
  let int = 15
  assert 0b00001111 == int
}

pub fn lex_1_test() {
  assert 1 - 1 == 0
}

pub fn lex_2_test() {
  let a = 1
  assert a - 1 == 0
}

pub fn lex_3_test() {
  assert 1 - 1 == 0
}

pub fn division_1_test() {
  assert 1 / 1 == 1
}

pub fn division_2_test() {
  assert 1 / 0 == 0
}

pub fn division_3_test() {
  assert 3 / 2 == 1
}

pub fn division_4_test() {
  assert 3 / 0 == 0
}
