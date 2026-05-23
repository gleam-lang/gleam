fn identity(x: a) -> a {
  x
}

fn add_one(x: Int) -> Int {
  x + 1
}

fn add_two(x: Int) -> Int {
  x + 2
}

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

// https://github.com/gleam-lang/gleam/issues/5743
pub fn no_duplicate_pipe_variable_with_case_in_pipeline_test() {
  let result =
    0
    |> case True {
      True -> add_one |> identity
      False -> add_two
    }
    |> add_one
  assert result == 2
}
