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

#[test]
pub fn long_chain_mixing_operators() {
    assert_format!(
        r#"pub fn main() {
  variable + variable - variable * variable / variable
  == variable * variable / variable - variable + variable
  || foo * bar > 11
}
"#
    );

    assert_format!(
        r#"pub fn main() {
  variable +. variable -. variable *. variable /. variable
  == variable *. variable /. variable -. variable +. variable
  || foo *. bar >=. 11
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

// https://discord.com/channels/768594524158427167/1187508793945378847/1187508793945378847
#[test]
fn binary_operation_in_assignment_that_is_almost_80_chars() {
    assert_format!(
        r#"pub fn main() {
  let is_vr_implicit =
    dicom_read_context.transfer_syntax == transfer_syntax.ImplicitVrLittleEndian
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2480
#[test]
fn labelled_field_with_binary_operators_are_not_broken_if_they_can_fit() {
    assert_format!(
        r#"pub fn main() {
  Ok(Lesson(
    name: names.name,
    text: text,
    code: code,
    path: chapter_path <> "/",
    previous: None,
    next: None,
  ))
}
"#
    );

    assert_format!(
        r#"pub fn main() {
  Ok(Lesson(
    name: names.name,
    text: text,
    code: code,
    path: chapter_path
      <> "/"
      <> this_one_doesnt_fit
      <> "and ends up on multiple lines",
    previous: None,
    next: None,
  ))
}
"#
    );

    assert_format!(
        r#"pub fn main() {
  Ok(foo(
    name: names.name,
    text: text,
    code: code,
    path: chapter_path <> "/",
    previous: None,
    next: None,
  ))
}
"#
    );

    assert_format!(
        r#"pub fn main() {
  Ok(foo(
    name: names.name,
    text: text,
    code: code,
    path: chapter_path
      <> "/"
      <> this_one_doesnt_fit
      <> "and ends up on multiple lines",
    previous: None,
    next: None,
  ))
}
"#
    );
}

#[test]
fn math_binops_kept_on_a_single_line_in_pipes() {
    assert_format!(
        r#"pub fn main() {
  1 + 2 * 3 / 4 - 5
  |> foo
  |> bar
}
"#
    );

    assert_format!(
        r#"pub fn main() {
  1 +. 2 *. 3 /. 4 -. 5
  |> foo
  |> bar
}
"#
    );
}

#[test]
fn binop_used_as_function_arguments_gets_nested() {
    assert_format!(
        r#"pub fn main() {
  foo(
    a_variable_with_a_long_name
      <> another_variable_with_a_long_name
      <> yet_another_variable_with_a_long_name,
    bar,
  )
}
"#
    );
}

#[test]
fn binop_is_not_nested_if_the_only_argument() {
    assert_format!(
        r#"pub fn main() {
  foo(
    a_variable_with_a_long_name
    <> another_variable_with_a_long_name
    <> yet_another_variable_with_a_long_name,
  )
}
"#
    );
}

#[test]
fn binop_inside_list_gets_nested() {
    assert_format!(
        r#"pub fn main() {
  [
    foo,
    a_variable_with_a_long_name
      <> another_variable_with_a_long_name
      <> yet_another_variable_with_a_long_name,
  ]
}
"#
    );
}

#[test]
fn binop_inside_list_is_not_nested_if_only_item() {
    assert_format!(
        r#"pub fn main() {
  [
    a_variable_with_a_long_name
    <> another_variable_with_a_long_name
    <> yet_another_variable_with_a_long_name,
  ]
}
"#
    );
}

#[test]
fn binop_inside_tuple_gets_nested() {
    assert_format!(
        r#"pub fn main() {
  #(
    foo,
    a_variable_with_a_long_name
      <> another_variable_with_a_long_name
      <> yet_another_variable_with_a_long_name,
  )
}
"#
    );
}

#[test]
fn binop_inside_tuple_is_not_nested_if_only_item() {
    assert_format!(
        r#"pub fn main() {
  #(
    a_variable_with_a_long_name
    <> another_variable_with_a_long_name
    <> yet_another_variable_with_a_long_name,
  )
}
"#
    );
}
