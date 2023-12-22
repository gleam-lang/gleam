use crate::assert_format;

// https://github.com/gleam-lang/gleam/issues/835
#[test]
pub fn long_binary_operation_sequence() {
    assert_format!(
        r#"pub fn main() {
  int.to_string(color.red)
  <> ", "
  <> int.to_string(color.green)
  <> ", "
  <> int.to_string(color.blue)
  <> ", "
  <> float.to_string(color.alpha)
}
"#
    );
}

#[test]
pub fn long_comparison_chain() {
    assert_format!(
        r#"pub fn main() {
  trying_a_comparison(this, is, a, function) > with_ints
  && trying_other_comparisons < with_ints
  || trying_other_comparisons <= with_ints
  && trying_other_comparisons >= with_ints
  || and_now_an_equality_check == with_a_function(foo, bar)
  && trying_other_comparisons >. with_floats
  || trying_other_comparisons <. with_floats(baz)
  && trying_other_comparisons <=. with_floats
  || trying_other_comparisons(foo, bar) >=. with_floats
  && foo <> bar
}
"#
    );
}

// Thanks Hayleigh for pointing this out!
#[test]
fn case_branch_is_not_broken_if_can_fit_on_line() {
    assert_format!(
        r#"pub fn main() {
  case remainder {
    _ if remainder >=. 0.5 && x >=. 0.0 ->
      float_sign(x) *. truncate_float(xabs +. 1.0) /. p
    _ -> float_sign(x) *. xabs_truncated /. p
  }
}
"#
    );
}
