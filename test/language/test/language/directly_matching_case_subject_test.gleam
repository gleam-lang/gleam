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
  let x =
    case #(1, 2) {
      #(1, b) -> b
      #(_, b) -> b
    }
  let #(a, _b) = #(x, 3)
  assert a == 2
}
