use crate::{assert_format, assert_format_rewrite};

#[test]
fn capture_with_single_argument() {
    assert_format_rewrite!(
        "pub fn main() -> Nil {
  wibble([], wobble(_))
}
",
        "pub fn main() -> Nil {
  wibble([], wobble)
}
"
    );
}

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
fn anonymous_function_as_final_function_argument() {
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
fn anonymous_function_with_single_line_body_as_final_function_argument() {
    assert_format!(
        r#"pub fn main() {
  some_function(123, 456, fn(x) { x })
}
"#
    );
}

#[test]
fn anonymous_function_with_multi_line_unbreakable_body_as_final_function_argument() {
    assert_format!(
        r#"pub fn main() {
  some_function(123, 456, fn(x) {
    call_to_other_function(a, b, c, d, e, f, g, h)
  })
}
"#
    );
}

#[test]
fn anonymous_function_with_multi_line_breakable_body_as_final_function_argument() {
    assert_format!(
        r#"pub fn main() {
  some_function(123, 456, fn(x) {
    call_to_other_function(a, b, c, d, e, f, g, { x + x })
  })
}
"#
    );
}

#[test]
fn anonymous_function_with_multi_line_long_breakable_body_as_final_function_argument() {
    assert_format!(
        r#"pub fn main() {
  some_function(123, 456, fn(x) {
    call_to_other_function(a, b, c, d, e, f, g, case wibble {
      Wibble -> 1
      Wobble -> 2
    })
  })
}
"#
    );
}

#[test]
fn function_call_as_final_function_argument_goes_on_its_own_line() {
    assert_format!(
        r#"pub fn main() {
  some_function_with_a_long_name(
    123,
    456,
    another_function_being_called(123, 456),
  )
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

#[test]
fn nested_breakable_lists_in_function_calls() {
    assert_format!(
        r#"pub fn main() {
  html([attribute("lang", "en")], [
    head([attribute("wibble", "wobble")], [
      title([], [text("Hello this is some HTML")]),
    ]),
    body([], [h1([], [text("Hello, world!")])]),
  ])
}
"#
    );
}

#[test]
fn nested_breakable_tuples_in_function_calls() {
    assert_format!(
        r#"pub fn main() {
  html(#(attribute("lang", "en")), #(
    head(#(attribute("wibble", "wobble")), #(
      title(#(), #(text("Hello this is some HTML"))),
      body(#(), #(text("Hello this is some HTML"))),
    )),
    body(#(), #(h1(#(), #(text("Hello, lisp!"))))),
  ))
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2435
#[test]
fn only_last_argument_can_be_broken() {
    assert_format!(
        r#"pub fn main() {
  tbd.workbook(for: "my_project")
  |> task(
    doc: "Run the project tests",
    tags: list.concat([["test", "ci"], gleam, typescript]),
    action: fn(_, _) { tbd.command(run: "gleam", with: ["test"]) },
  )
  |> run
}
"#
    );

    assert_format!(
        r#"pub fn main() {
  Theme(
    flag: styler([32]),
    heading: styler([1, 95]),
    highlight: styler([1, 36]),
    parameter: styler([34]),
    tag: styler([33]),
    given_tag: styler([3]),
    first_tag: styler([1]),
    tab: "    ",
  )
}
"#
    );
}

#[test]
fn function_that_is_a_little_over_the_limit() {
    assert_format!(
        r#"pub fn handle_request(
  handler: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,
) -> Nil {
  todo
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2571
#[test]
fn expr_function_as_last_argument() {
    assert_format!(
        r#"pub fn main() {
  Builder(
    accumulator: "",
    update: fn(accum, val) { accum <> val },
    final: fn(accum) { accum },
  )
}
"#
    );

    // We want to make sure that, if it goes over the limit NOT with its
    // arguments' list the body is still the first thing that gets split.
    assert_format!(
        r#"pub fn main() {
  Builder(accumulator: "", update: fn(accum, val) { accum }, final: fn(accum) {
    accum
  })
}
"#
    );
}

#[test]
fn comment_at_start_of_inline_function_body() {
    assert_format!(
        r#"pub fn main() {
  let add = fn(x: Int, y: Int) {
    // This is a comment
    x + y
  }
}
"#
    );
}

#[test]
fn comment_at_start_of_top_level_function_body() {
    assert_format!(
        r#"pub fn add(x: Int, y: Int) {
  // This is a comment
  x + y
}
"#
    );
}

#[test]
fn comment_at_end_of_inline_function_args() {
    assert_format!(
        r#"pub fn main() {
  let add = fn(
    x: Int,
    y: Int,
    // This is a comment
  ) {
    x + y
  }
}
"#
    );
}

#[test]
fn comment_middle_of_inline_function_body() {
    assert_format!(
        r#"pub fn main() {
  let add = fn(x: Int, y: Int, z: Int) {
    let a = x + y
    // This is a comment
    a + z
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/5004
#[test]
fn comment_in_tuple_return_type() {
    assert_format_rewrite!(
        r#"pub fn main() -> #(
  // This is a string
  String, // This is an awesome string
) {
  todo
}
"#,
        r#"pub fn main() -> #(
  String,
  // This is a string
  // This is an awesome string
) {
  todo
}
"#
    );
}
