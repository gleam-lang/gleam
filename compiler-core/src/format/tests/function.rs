use crate::assert_format;

#[test]
fn deprecated() {
    assert_format!(
        r#"@deprecated("use something else instead")
pub fn main() -> Nil {
  Nil
}
"#
    );
}

#[test]
fn deprecated_external() {
    assert_format!(
        r#"@deprecated("use something else instead")
@external(erlang, "thing", "main")
pub fn main() -> Nil
"#
    );
}

#[test]
fn lambda_as_final_function_argument() {
    assert_format!(
        r#"pub fn main() {
  some_function(123, 456, fn(x) {
    let y = x + 1
    y
  })
}
"#
    );
}

#[test]
fn function_call_as_final_function_argument() {
    assert_format!(
        r#"pub fn main() {
  some_function_with_a_long_name(123, 456, another_function_being_called(
    123,
    456,
  ))
}
"#
    );
}

#[test]
fn tuple_as_final_function_argument() {
    assert_format!(
        r#"pub fn main() {
  some_function(123, 456, #(
    "Here is a very long string which causes the formatter to wrap it",
  ))
}
"#
    );
}

#[test]
fn list_as_final_function_argument() {
    assert_format!(
        r#"pub fn main() {
  some_function(123, 456, [
    "Here is a very long string which causes the formatter to wrap it",
  ])
}
"#
    );
}

#[test]
fn case_expression_as_final_function_argument() {
    assert_format!(
        r#"pub fn main() {
  some_function(123, 456, case my_var {
    True -> True
    False -> False
  })
}
"#
    );
}

#[test]
fn block_as_final_function_argument() {
    assert_format!(
        r#"pub fn main() {
  some_function(123, 456, {
    let days = 7
    days * 24 * 60 * 60
  })
}
"#
    );
}

#[test]
fn when_all_arguments_are_too_long_each_one_is_on_its_own_line() {
    assert_format!(
        r#"pub fn main() {
  some_function(
    variable_with_really_long_name,
    whoah_this_is_getting_out_of_hand,
    ["Here is a very long string which causes the formatter to wrap it"],
  )
}
"#
    );
}
