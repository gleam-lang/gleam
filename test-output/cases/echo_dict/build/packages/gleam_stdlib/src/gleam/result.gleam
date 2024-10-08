//// Result represents the result of something that may succeed or not.
//// `Ok` means it was successful, `Error` means it was not successful.

import gleam/list

/// Checks whether the result is an `Ok` value.
///
/// ## Examples
///
/// ```gleam
/// is_ok(Ok(1))
/// // -> True
/// ```
///
/// ```gleam
/// is_ok(Error(Nil))
/// // -> False
/// ```
///
pub fn is_ok(result: Result(a, e)) -> Bool {
  case result {
    Error(_) -> False
    Ok(_) -> True
  }
}

/// Checks whether the result is an `Error` value.
///
/// ## Examples
///
/// ```gleam
/// is_error(Ok(1))
/// // -> False
/// ```
///
/// ```gleam
/// is_error(Error(Nil))
/// // -> True
/// ```
///
pub fn is_error(result: Result(a, e)) -> Bool {
  case result {
    Ok(_) -> False
    Error(_) -> True
  }
}

/// Updates a value held within the `Ok` of a result by calling a given function
/// on it.
///
/// If the result is an `Error` rather than `Ok` the function is not called and the
/// result stays the same.
///
/// ## Examples
///
/// ```gleam
/// map(over: Ok(1), with: fn(x) { x + 1 })
/// // -> Ok(2)
/// ```
///
/// ```gleam
/// map(over: Error(1), with: fn(x) { x + 1 })
/// // -> Error(1)
/// ```
///
pub fn map(over result: Result(a, e), with fun: fn(a) -> b) -> Result(b, e) {
  case result {
    Ok(x) -> Ok(fun(x))
    Error(e) -> Error(e)
  }
}

/// Updates a value held within the `Error` of a result by calling a given function
/// on it.
///
/// If the result is `Ok` rather than `Error` the function is not called and the
/// result stays the same.
///
/// ## Examples
///
/// ```gleam
/// map_error(over: Error(1), with: fn(x) { x + 1 })
/// // -> Error(2)
/// ```
///
/// ```gleam
/// map_error(over: Ok(1), with: fn(x) { x + 1 })
/// // -> Ok(1)
/// ```
///
pub fn map_error(
  over result: Result(a, e),
  with fun: fn(e) -> f,
) -> Result(a, f) {
  case result {
    Ok(x) -> Ok(x)
    Error(error) -> Error(fun(error))
  }
}

/// Merges a nested `Result` into a single layer.
///
/// ## Examples
///
/// ```gleam
/// flatten(Ok(Ok(1)))
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// flatten(Ok(Error("")))
/// // -> Error("")
/// ```
///
/// ```gleam
/// flatten(Error(Nil))
/// // -> Error(Nil)
/// ```
///
pub fn flatten(result: Result(Result(a, e), e)) -> Result(a, e) {
  case result {
    Ok(x) -> x
    Error(error) -> Error(error)
  }
}

/// "Updates" an `Ok` result by passing its value to a function that yields a result,
/// and returning the yielded result. (This may "replace" the `Ok` with an `Error`.)
///
/// If the input is an `Error` rather than an `Ok`, the function is not called and
/// the original `Error` is returned.
///
/// This function is the equivalent of calling `map` followed by `flatten`, and
/// it is useful for chaining together multiple functions that may fail.
///
/// ## Examples
///
/// ```gleam
/// try(Ok(1), fn(x) { Ok(x + 1) })
/// // -> Ok(2)
/// ```
///
/// ```gleam
/// try(Ok(1), fn(x) { Ok(#("a", x)) })
/// // -> Ok(#("a", 1))
/// ```
///
/// ```gleam
/// try(Ok(1), fn(_) { Error("Oh no") })
/// // -> Error("Oh no")
/// ```
///
/// ```gleam
/// try(Error(Nil), fn(x) { Ok(x + 1) })
/// // -> Error(Nil)
/// ```
///
pub fn try(
  result: Result(a, e),
  apply fun: fn(a) -> Result(b, e),
) -> Result(b, e) {
  case result {
    Ok(x) -> fun(x)
    Error(e) -> Error(e)
  }
}

/// An alias for `try`. See the documentation for that function for more information.
///
pub fn then(
  result: Result(a, e),
  apply fun: fn(a) -> Result(b, e),
) -> Result(b, e) {
  try(result, fun)
}

/// Extracts the `Ok` value from a result, returning a default value if the result
/// is an `Error`.
///
/// ## Examples
///
/// ```gleam
/// unwrap(Ok(1), 0)
/// // -> 1
/// ```
///
/// ```gleam
/// unwrap(Error(""), 0)
/// // -> 0
/// ```
///
pub fn unwrap(result: Result(a, e), or default: a) -> a {
  case result {
    Ok(v) -> v
    Error(_) -> default
  }
}

/// Extracts the `Ok` value from a result, evaluating the default function if the result
/// is an `Error`.
///
/// ## Examples
///
/// ```gleam
/// lazy_unwrap(Ok(1), fn() { 0 })
/// // -> 1
/// ```
///
/// ```gleam
/// lazy_unwrap(Error(""), fn() { 0 })
/// // -> 0
/// ```
///
pub fn lazy_unwrap(result: Result(a, e), or default: fn() -> a) -> a {
  case result {
    Ok(v) -> v
    Error(_) -> default()
  }
}

/// Extracts the `Error` value from a result, returning a default value if the result
/// is an `Ok`.
///
/// ## Examples
///
/// ```gleam
/// unwrap_error(Error(1), 0)
/// // -> 1
/// ```
///
/// ```gleam
/// unwrap_error(Ok(""), 0)
/// // -> 0
/// ```
///
pub fn unwrap_error(result: Result(a, e), or default: e) -> e {
  case result {
    Ok(_) -> default
    Error(e) -> e
  }
}

/// Extracts the inner value from a result. Both the value and error must be of
/// the same type.
///
/// ## Examples
///
/// ```gleam
/// unwrap_both(Error(1))
/// // -> 1
/// ```
///
/// ```gleam
/// unwrap_both(Ok(2))
/// // -> 2
/// ```
///
pub fn unwrap_both(result: Result(a, a)) -> a {
  case result {
    Ok(a) -> a
    Error(a) -> a
  }
}

/// Transforms any error into `Error(Nil)`.
///
/// ## Examples
///
/// ```gleam
/// nil_error(Error(1))
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// nil_error(Ok(1))
/// // -> Ok(1)
/// ```
///
pub fn nil_error(result: Result(a, e)) -> Result(a, Nil) {
  map_error(result, fn(_) { Nil })
}

/// Returns the first value if it is `Ok`, otherwise returns the second value.
///
/// ## Examples
///
/// ```gleam
/// or(Ok(1), Ok(2))
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// or(Ok(1), Error("Error 2"))
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// or(Error("Error 1"), Ok(2))
/// // -> Ok(2)
/// ```
///
/// ```gleam
/// or(Error("Error 1"), Error("Error 2"))
/// // -> Error("Error 2")
/// ```
///
pub fn or(first: Result(a, e), second: Result(a, e)) -> Result(a, e) {
  case first {
    Ok(_) -> first
    Error(_) -> second
  }
}

/// Returns the first value if it is `Ok`, otherwise evaluates the given function for a fallback value.
///
/// If you need access to the initial error value, use `result.try_recover`.
///
/// ## Examples
///
/// ```gleam
/// lazy_or(Ok(1), fn() { Ok(2) })
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// lazy_or(Ok(1), fn() { Error("Error 2") })
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// lazy_or(Error("Error 1"), fn() { Ok(2) })
/// // -> Ok(2)
/// ```
///
/// ```gleam
/// lazy_or(Error("Error 1"), fn() { Error("Error 2") })
/// // -> Error("Error 2")
/// ```
///
pub fn lazy_or(
  first: Result(a, e),
  second: fn() -> Result(a, e),
) -> Result(a, e) {
  case first {
    Ok(_) -> first
    Error(_) -> second()
  }
}

/// Combines a list of results into a single result.
/// If all elements in the list are `Ok` then returns an `Ok` holding the list of values.
/// If any element is `Error` then returns the first error.
///
/// ## Examples
///
/// ```gleam
/// all([Ok(1), Ok(2)])
/// // -> Ok([1, 2])
/// ```
///
/// ```gleam
/// all([Ok(1), Error("e")])
/// // -> Error("e")
/// ```
///
pub fn all(results: List(Result(a, e))) -> Result(List(a), e) {
  list.try_map(results, fn(x) { x })
}

/// Given a list of results, returns a pair where the first element is a list
/// of all the values inside `Ok` and the second element is a list with all the
/// values inside `Error`. The values in both lists appear in reverse order with
/// respect to their position in the original list of results.
///
/// ## Examples
///
/// ```gleam
/// partition([Ok(1), Error("a"), Error("b"), Ok(2)])
/// // -> #([2, 1], ["b", "a"])
/// ```
///
pub fn partition(results: List(Result(a, e))) -> #(List(a), List(e)) {
  do_partition(results, [], [])
}

fn do_partition(results: List(Result(a, e)), oks: List(a), errors: List(e)) {
  case results {
    [] -> #(oks, errors)
    [Ok(a), ..rest] -> do_partition(rest, [a, ..oks], errors)
    [Error(e), ..rest] -> do_partition(rest, oks, [e, ..errors])
  }
}

/// Replace the value within a result
///
/// ## Examples
///
/// ```gleam
/// replace(Ok(1), Nil)
/// // -> Ok(Nil)
/// ```
///
/// ```gleam
/// replace(Error(1), Nil)
/// // -> Error(1)
/// ```
///
pub fn replace(result: Result(a, e), value: b) -> Result(b, e) {
  case result {
    Ok(_) -> Ok(value)
    Error(error) -> Error(error)
  }
}

/// Replace the error within a result
///
/// ## Examples
///
/// ```gleam
/// replace_error(Error(1), Nil)
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// replace_error(Ok(1), Nil)
/// // -> Ok(1)
/// ```
///
pub fn replace_error(result: Result(a, e), error: f) -> Result(a, f) {
  case result {
    Ok(x) -> Ok(x)
    Error(_) -> Error(error)
  }
}

/// Given a list of results, returns only the values inside `Ok`.
///
/// ## Examples
///
/// ```gleam
/// values([Ok(1), Error("a"), Ok(3)])
/// // -> [1, 3]
/// ```
///
pub fn values(results: List(Result(a, e))) -> List(a) {
  list.filter_map(results, fn(r) { r })
}

/// Updates a value held within the `Error` of a result by calling a given function
/// on it, where the given function also returns a result. The two results are
/// then merged together into one result.
///
/// If the result is an `Ok` rather than `Error` the function is not called and the
/// result stays the same.
///
/// This function is useful for chaining together computations that may fail
/// and trying to recover from possible errors.
///
/// If you do not need access to the initial error value, use `result.lazy_or`.
///
/// ## Examples
///
/// ```gleam
/// Ok(1) |> try_recover(with: fn(_) { Error("failed to recover") })
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// Error(1) |> try_recover(with: fn(error) { Ok(error + 1) })
/// // -> Ok(2)
/// ```
///
/// ```gleam
/// Error(1) |> try_recover(with: fn(error) { Error("failed to recover") })
/// // -> Error("failed to recover")
/// ```
///
pub fn try_recover(
  result: Result(a, e),
  with fun: fn(e) -> Result(a, f),
) -> Result(a, f) {
  case result {
    Ok(value) -> Ok(value)
    Error(error) -> fun(error)
  }
}
