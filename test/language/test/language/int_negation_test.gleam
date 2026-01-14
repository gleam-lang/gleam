pub fn negation_1_test() {
  let a = 3
  let b = -a
  assert -3 == b
}

pub fn negation_2_test() {
  let a = 3
  let b = -{ -a }
  assert 3 == b
}

pub fn negation_3_test() {
  let a = 3
  let b = -{ -{ -a } }
  assert -3 == b
}

pub fn negation_4_test() {
  let a = 3
  let b = -a
  let c = a - -b
  assert 0 == c
}

pub fn negation_5_test() {
  let a = 3
  let b = -a
  let c = a - -{ -{ -{ -{ -b } } } }
  assert 0 == c
}

pub fn negation_6_test() {
  let a = 3
  let b = -a
  let c = -a - -b
  assert -6 == c
}

pub fn negation_7_test() {
  let abs = fn(value) {
    case value {
      value if value > 0 -> value
      _ -> -value
    }
  }
  assert -6 == -abs(-6)
}
