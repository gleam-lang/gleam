use crate::assert_erl;

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

pub fn then(result: Result(a, e), apply f: fn(a) -> Result(b, e)) -> Result(b, e) {
  try(result, f)
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
    assert_erl!(
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
    assert_erl!(
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
    assert_erl!(
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
    // `result.then` calls `result.try`, meaning this must be inlined twice to
    // achieve the desired result.
    assert_erl!(
        ("gleam_stdlib", "gleam/result", RESULT_MODULE),
        "
import gleam/result

pub fn main() {
  result.then(Ok(10), Error)
}
"
    );
}

#[test]
fn inline_function_with_use() {
    assert_erl!(
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
    assert_erl!(
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
    assert_erl!(
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
    // We just use `bool.guard` as the name here because it will be inlined
    assert_erl!(
        (
            "gleam_stdlib",
            "gleam/bool",
            "
pub fn guard(something) {
  case something {
    True -> something
    False -> False
  }
}
"
        ),
        "
import gleam/bool

pub fn main() {
  bool.guard(True)
}
"
    );
}

#[test]
fn do_not_inline_parameters_that_have_side_effects() {
    assert_erl!(
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
    assert_erl!(
        "
pub fn main() {
  fn(a, b) { #(a, b) }(42, False)
}
"
    );
}

#[test]
fn inline_anonymous_function_in_pipe() {
    assert_erl!(
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
    assert_erl!(
        "
pub fn main() {
  1 |> add(4, _)
}

fn add(a, b) { a + b }
"
    );
}
