pub fn negation_true_test() {
  assert !True == False
}

pub fn negation_false_test() {
  assert !False == True
}

// This would crash if the right hand side evaluated
pub fn negation_true_and_panic_test() {
  let bool = !True
  bool && panic
}

// This would crash if the right hand side evaluated
pub fn negation_false_or_panic_test() {
  let bool = !False
  bool || panic
}
