// github.com/gleam-lang/gleam/issues/5265
pub fn directly_matching_case_subject_test() {
  let result = {
    let x = "ABC"
    case True {
      True -> {
        let x = 79
        0
      }
      False -> {
        let x = True
        0
      }
    }
    x
  }

  assert result == "ABC"
}

// github.com/gleam-lang/gleam/issues/5467
pub fn no_duplicate_let_after_directly_matching_case_test() {
  let x = case #(1, 2) {
    #(1, b) -> b
    #(_, b) -> b
  }
  let #(a, _b) = #(x, 3)
  assert a == 2
}

// https://github.com/gleam-lang/gleam/issues/5612
pub fn no_duplicate_let_after_case_with_same_named_variable_test() {
  let x = case #(1, 2) {
    #(_, x) -> x
  }
  assert x == 2
}

// https://github.com/gleam-lang/gleam/issues/5612
pub fn no_duplicate_let_after_case_with_same_named_variable_with_declaration_before_and_after_test() {
  let x = 0
  let x = x
  let x = case #(1, 2) {
    #(_, x) -> x
  }
  let x = x
  assert x == 2
}
