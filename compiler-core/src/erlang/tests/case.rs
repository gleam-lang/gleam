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

// https://github.com/gleam-lang/gleam/issues/1927
#[test]
fn discard_list_tail() {
    assert_erl!(
        "
pub fn unsafe_head(xs) {
  assert [x, ..] = xs
  x
}
"
    )
}
