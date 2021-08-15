use crate::assert_js;

use super::CURRENT_PACKAGE;

#[test]
fn exported_functions() {
    assert_js!(
        r#"
pub fn add(x, y) {
    x + y
}"#,
    );
}

#[test]
fn calling_functions() {
    assert_js!(
        r#"
pub fn twice(f: fn(t) -> t, x: t) -> t {
  f(f(x))
}
pub fn add_one(x: Int) -> Int {
  x + 1
}
pub fn add_two(x: Int) -> Int {
  twice(add_one, x)
}

pub fn take_two(x: Int) -> Int {
  twice(fn(y) {y - 1}, x)
}
"#,
    );
}

#[test]
fn function_formatting() {
    assert_js!(
        r#"
pub fn add(the_first_variable_that_should_be_added, the_second_variable_that_should_be_added) {
  the_first_variable_that_should_be_added + the_second_variable_that_should_be_added
}"#,
    );

    assert_js!(
        r#"
pub fn this_function_really_does_have_a_ludicrously_unfeasibly_long_name_for_a_function(x, y) {
x + y
}"#,
    );

    assert_js!(
        r#"
pub fn add(x, y) {
x + y
}

pub fn long() {
  add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, 1)))))))))))))))
}"#,
    );

    assert_js!(
        r#"
pub fn math(x, y) {
  fn() {
    x + y
    x - y
    2 * x
  }
}"#,
    );
}

#[test]
fn tail_call() {
    assert_js!(
        r#"
pub fn count(xs, n) {
  case xs {
    [] -> n
    [_, ..xs] -> count(xs, n + 1)
  }
}
"#,
    );
}

#[test]
fn tail_call_doesnt_clobber_tail_position_tracking() {
    assert_js!(
        r#"
pub fn loop(indentation) {
  case indentation > 0 {
    True -> loop(indentation - 1)
    False -> Nil
  }
}
"#,
    );
}

#[test]
fn pipe_last() {
    assert_js!(
        r#"fn id(x) { x }
pub fn main() {
  1
  |> id
}
"#,
    );
}

#[test]
fn calling_fn_literal() {
    assert_js!(
        r#"pub fn main() {
  fn(x) { x }(1)
}
"#,
    );
}

// Don't mistake calling a function with the same name as the current function
// as tail recursion
#[test]
fn shadowing_current() {
    assert_js!(
        r#"pub fn main() {
  let main = fn() { 0 }
  main()
}
"#,
    );
}

#[test]
fn recursion_with_discards() {
    assert_js!(
        r#"pub fn main(f, _) {
  f()
  main(f, 1)
}
"#,
    );
}

#[test]
fn no_recur_in_anon_fn() {
    assert_js!(
        r#"pub fn main() {
  fn() { main() }
  1
}
"#,
    );
}

#[test]
fn case_in_call() {
    assert_js!(
        r#"pub fn main(f, x) {
  f(case x {
    1 -> 2
    _ -> 0
  })
}
"#,
    );
}

#[test]
fn reserved_word_fn() {
    assert_js!(
        r#"pub fn class() {
  Nil
}
"#,
    );
}

#[test]
fn reserved_word_imported() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["for".to_string()],
            "pub fn class() { 1 }"
        ),
        r#"import for.{class}

pub fn export() {
  class()
}
"#,
        r#"import * as For from "../for.js";
import { class$ } from "../for.js";

export function export$() {
  return class$();
}
"#
    );
}

#[test]
fn reserved_word_imported_alias() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["for".to_string()],
            "pub fn class() { 1 }"
        ),
        r#"import for.{class as while} as function

pub fn export() {
  let delete = function.class
  while()
}
"#,
        r#"import * as Function from "../for.js";
import { class$ as while$ } from "../for.js";

export function export$() {
  let delete$ = Function.class$;
  return while$();
}
"#
    );
}

#[test]
fn reserved_word_const() {
    assert_js!(
        r#"const in = 1

pub fn export() {
  in
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1208
#[test]
fn reserved_word_argument() {
    assert_js!(
        r#"pub fn main(with) {
  with
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1186
#[test]
fn multiple_discard() {
    assert_js!(
        r#"pub fn main(_, _, _) {
  1
}
"#,
    );
}

#[test]
fn keyword_in_recursive_function() {
    assert_js!(
        r#"pub fn main(with: Int) -> Nil {
  main(with - 1)
}
"#,
    );
}

#[test]
fn reserved_word_in_function_arguments() {
    assert_js!(
        r#"pub fn main(arguments, eval) {
  #(arguments, eval)
}
"#,
    );
}
