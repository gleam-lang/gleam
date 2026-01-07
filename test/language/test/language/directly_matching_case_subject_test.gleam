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
