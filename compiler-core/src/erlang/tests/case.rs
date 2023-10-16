use crate::assert_erl;

// https://github.com/gleam-lang/gleam/issues/1675
#[test]
fn alternative_pattern_variable_rewriting() {
    assert_erl!(
        "
pub fn myfun(mt) {
  case mt {
    1 | _ ->
      1
      |> Ok
  }
  1
  |> Ok
}
"
    )
}

// https://github.com/gleam-lang/gleam/issues/2349
#[test]
fn positive_zero_pattern() {
    assert_erl!(
        "
pub fn main(x) {
  case x {
    0.0 -> 1
    _ -> 2
  }
}
"
    )
}

// https://github.com/gleam-lang/gleam/issues/2349
#[test]
fn negative_zero_pattern() {
    assert_erl!(
        "
pub fn main(x) {
  case x {
    -0.0 -> 1
    _ -> 2
  }
}
"
    )
}
