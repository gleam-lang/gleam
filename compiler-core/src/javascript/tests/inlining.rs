use crate::assert_js;

const BOOL_MODULE: &str = "
pub fn guard(
  when condition: Bool,
  return value: a,
  otherwise callback: fn() -> a,
) -> a {
  case condition {
    True -> value
    False -> callback()
  }
}

pub fn lazy_guard(
  when condition: Bool,
  return consequence: fn() -> a,
  otherwise alternative: fn() -> a,
) -> a {
  case condition {
    True -> consequence()
    False -> alternative()
  }
}
";

const RESULT_MODULE: &str = "
pub fn try(result: Result(a, e), apply f: fn(a) -> Result(b, e)) -> Result(b, e) {
  case result {
    Ok(value) -> f(value)
    Error(error) -> Error(error)
  }
}

pub fn map(over result: Result(a, e), with f: fn(a) -> b) -> Result(b, e) {
  case result {
    Ok(value) -> Ok(f(value))
    Error(error) -> Error(error)
  }
}
";

#[test]
fn inline_higher_order_function() {
    assert_js!(
        ("gleam_stdlib", "gleam/result", RESULT_MODULE),
        "
import gleam/result

pub fn main() {
  result.map(over: Ok(10), with: double)
}

fn double(x) { x + x }
"
    );
}

#[test]
fn inline_higher_order_function_with_capture() {
    assert_js!(
        ("gleam_stdlib", "gleam/result", RESULT_MODULE),
        "
import gleam/result

pub fn main() {
  result.try(Ok(10), divide(_, 2))
}

fn divide(a: Int, b: Int) -> Result(Int, Nil) {
  case a % b {
    0 -> Ok(a / b)
    _ -> Error(Nil)
  }
}
"
    );
}

#[test]
fn inline_higher_order_function_anonymous() {
    assert_js!(
        ("gleam_stdlib", "gleam/result", RESULT_MODULE),
        "
import gleam/result

pub fn main() {
  result.try(Ok(10), fn(value) {
    Ok({ value + 2 } * 4)
  })
}
"
    );
}

#[test]
fn inline_function_which_calls_other_function() {
    // This function calls `result.try`, meaning this must be inlined twice to
    // achieve the desired result.
    assert_js!(
        ("gleam_stdlib", "gleam/result", RESULT_MODULE),
        (
            "gleam_stdlib",
            "testing",
            "
import gleam/result.{try}

pub fn always_inline(result, f) -> Result(b, e) {
  try(result, f)
}
"
        ),
        "
import testing

pub fn main() {
  testing.always_inline(Ok(10), Error)
}
"
    );
}

#[test]
fn inline_function_with_use() {
    assert_js!(
        ("gleam_stdlib", "gleam/bool", BOOL_MODULE),
        "
import gleam/bool

pub fn divide(a, b) {
  use <- bool.guard(when: b == 0, return: 0)
  a / b
}
"
    );
}

#[test]
fn inline_function_with_use_and_anonymous() {
    assert_js!(
        ("gleam_stdlib", "gleam/bool", BOOL_MODULE),
        r#"
import gleam/bool

pub fn divide(a, b) {
  use <- bool.lazy_guard(b == 0, fn() { panic as "Cannot divide by 0" })
  a / b
}
"#
    );
}

#[test]
fn inline_function_with_use_becomes_tail_recursive() {
    assert_js!(
        ("gleam_stdlib", "gleam/bool", BOOL_MODULE),
        "
import gleam/bool

pub fn count(from: Int, to: Int) -> Int {
  use <- bool.guard(when: from >= to, return: from)
  echo from
  count(from + 1, to)
}
"
    );
}

#[test]
fn do_not_inline_parameters_used_more_than_once() {
    // Since the `something` parameter is used more than once in the body of the
    // function, it should not be inlined, and should be assigned once at the
    // beginning of the function.
    assert_js!(
        (
            "gleam_stdlib",
            "testing",
            "
pub fn always_inline(something) {
  case something {
    True -> something
    False -> False
  }
}
"
        ),
        "
import testing

pub fn main() {
  testing.always_inline(True)
}
"
    );
}

#[test]
fn do_not_inline_parameters_that_have_side_effects() {
    assert_js!(
        ("gleam_stdlib", "gleam/result", RESULT_MODULE),
        r#"
import gleam/result

pub fn main() {
  result.map(Ok(10), do_side_effects())
}

fn do_side_effects() {
  let function = fn(x) { x + 1 }
  panic as "Side effects"
  function
}
"#
    );
}

#[test]
fn inline_anonymous_function_call() {
    assert_js!(
        "
pub fn main() {
  fn(a, b) { #(a, b) }(42, False)
}
"
    );
}

#[test]
fn inline_anonymous_function_in_pipe() {
    assert_js!(
        "
pub fn main() {
  1 |> fn(x) { x + 1 } |> fn(y) { y * y }
}
"
    );
}

#[test]
fn inline_function_capture_in_pipe() {
    // The function capture is desugared to an anonymous function, so it should
    // be turned into a direct call to `add`
    assert_js!(
        "
pub fn main() {
  1 |> add(4, _)
}

fn add(a, b) { a + b }
"
    );
}

#[test]
fn inlining_works_through_blocks() {
    assert_js!(
        "
pub fn main() {
    { fn(x) { Ok(x + 1) } }(41)
}
"
    );
}

#[test]
fn blocks_get_preserved_when_needed() {
    assert_js!(
        "
pub fn main() {
    { 4 |> make_adder }(6)
}

fn make_adder(a) {
  fn(b) { a + b }
}
"
    );
}

#[test]
fn blocks_get_preserved_when_needed2() {
    assert_js!(
        "
pub fn main() {
    fn(x) { 1 + x }(2) * 3
}
"
    );
}

#[test]
fn parameters_from_nested_functions_are_correctly_inlined() {
    assert_js!(
        ("gleam_stdlib", "gleam/result", RESULT_MODULE),
        "
import gleam/result

pub fn halve_all(a, b, c) {
  use x <- result.try(divide(a, 2))
  use y <- result.try(divide(b, 2))
  use z <- result.map(divide(c, 2))

  #(x, y, z)
}

fn divide(a, b) {
  case a % b {
    0 -> Ok(a / b)
    _ -> Error(Nil)
  }
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4852
#[test]
fn inlining_works_properly_with_record_updates() {
    assert_js!(
        ("gleam_stdlib", "gleam/result", RESULT_MODULE),
        "
import gleam/result

pub type Wibble {
  Wibble(a: Int, b: Int)
}

pub fn main() {
  let w = Wibble(1, 2)
  use b <- result.map(Ok(3))
  Wibble(..w, b:)
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4877
#[test]
fn inline_shadowed_variable() {
    assert_js!(
        "
pub fn main() {
  let a = 10
  let b = 20

  fn(x) {
    let a = 7
    x + a
  }(a + b)

  a
}
"
    );
}

#[test]
fn inline_variable_shadowing_parameter() {
    assert_js!(
        "
pub fn sum(a, b) {
  fn(x) {
    let a = 7
    x + a
  }(a + b)

  a
}
"
    );
}

#[test]
fn inline_shadowed_variable_nested() {
    assert_js!(
        "
pub fn sum(a, b) {
  fn(x) {
    let a = 7
    fn(y) {
      let a = 10
      y - a
    }(x + a)

    a
  }(a + b)

  a
}
"
    );
}

#[test]
fn inline_variable_shadowed_in_case_pattern() {
    assert_js!(
        "
pub fn sum() {
  let a = 10
  let b = 20

  fn(x) {
    case 7, 8 {
      a, b -> a + b + x
    }
  }(a + b)

  a + b
}
"
    );
}

#[test]
fn inline_variable_shadowing_case_pattern() {
    assert_js!(
        "
pub fn sum() {
  case 1, 2 {
    a, b -> fn(x) {
      let a = 7
      x + a
    }(a + b)
  }
}
"
    );
}
