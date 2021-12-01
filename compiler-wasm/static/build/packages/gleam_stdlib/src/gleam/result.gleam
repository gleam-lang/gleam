//// Result represents the result of something that may succeed or not.
//// `Ok` means it was successful, `Error` means it was not successful.

import gleam/list

/// Checks whether the result is an Ok value.
///
/// ## Examples
///
///    > is_ok(Ok(1))
///    True
///
///    > is_ok(Error(Nil))
///    False
///
pub fn is_ok(result: Result(a, e)) -> Bool {
  case result {
    Error(_) -> False
    Ok(_) -> True
  }
}

/// Checks whether the result is an Error value.
///
/// ## Examples
///
///    > is_error(Ok(1))
///    False
///
///    > is_error(Error(Nil))
///    True
///
pub fn is_error(result: Result(a, e)) -> Bool {
  case result {
    Ok(_) -> False
    Error(_) -> True
  }
}

/// Updates a value held within the Ok of a result by calling a given function
/// on it.
///
/// If the result is an Error rather than OK the function is not called and the
/// result stays the same.
///
/// ## Examples
///
///    > map(over: Ok(1), with: fn(x) { x + 1 })
///    Ok(2)
///
///    > map(over: Error(1), with: fn(x) { x + 1 })
///    Error(1)
///
pub fn map(over result: Result(a, e), with fun: fn(a) -> b) -> Result(b, e) {
  case result {
    Ok(x) -> Ok(fun(x))
    Error(e) -> Error(e)
  }
}

/// Updates a value held within the Error of a result by calling a given function
/// on it.
///
/// If the result is Ok rather than Error the function is not called and the
/// result stays the same.
///
/// ## Examples
///
///    > map_error(over: Error(1), with: fn(x) { x + 1 })
///    Error(2)
///
///    > map_error(over: Ok(1), with: fn(x) { x + 1 })
///    Ok(1)
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

/// Merges a nested Result into a single layer.
///
/// ## Examples
///
///    > flatten(Ok(Ok(1)))
///    Ok(1)
///
///    > flatten(Ok(Error("")))
///    Error("")
///
///    > flatten(Error(Nil))
///    Error(Nil)
///
pub fn flatten(result: Result(Result(a, e), e)) -> Result(a, e) {
  case result {
    Ok(x) -> x
    Error(error) -> Error(error)
  }
}

/// Updates a value held within the Ok of a result by calling a given function
/// on it, where the given function also returns a result. The two results are
/// then merged together into one result.
///
/// If the result is an Error rather than OK the function is not called and the
/// result stays the same.
///
/// This function is the equivalent of calling `map` followed by `flatten`, and
/// it is useful for chaining together multiple functions that may fail.
///
/// ## Examples
///
///    > then(Ok(1), fn(x) { Ok(x + 1) })
///    Ok(2)
///
///    > then(Ok(1), fn(x) { Ok(#("a", x)) })
///    Ok(#("a", 1))
///
///    > then(Ok(1), fn(_) { Error("Oh no") })
///    Error("Oh no")
///
///    > then(Error(Nil), fn(x) { Ok(x + 1) })
///    Error(Nil)
///
pub fn then(
  result: Result(a, e),
  apply fun: fn(a) -> Result(b, e),
) -> Result(b, e) {
  case result {
    Ok(x) -> fun(x)
    Error(e) -> Error(e)
  }
}

/// Extracts the Ok value from a result, returning a default value if the result
/// is an Error.
///
/// ## Examples
///
///    > unwrap(Ok(1), 0)
///    1
///
///    > unwrap(Error(""), 0)
///    0
///
pub fn unwrap(result: Result(a, e), or default: a) -> a {
  case result {
    Ok(v) -> v
    Error(_) -> default
  }
}

/// Extracts the Ok value from a result, evaluating the default function if the result
/// is an Error.
///
/// ## Examples
///
///    > lazy_unwrap(Ok(1), fn() { 0 })
///    1
///
///    > lazy_unwrap(Error(""), fn() { 0 })
///    0
///
pub fn lazy_unwrap(result: Result(a, e), or default: fn() -> a) -> a {
  case result {
    Ok(v) -> v
    Error(_) -> default()
  }
}

/// Transforms any error into Error(Nil)
///
/// ## Examples
///
///    > nil_error(Error(1))
///    Error(Nil)
///
///    > nil_error(Ok(1))
///    Ok(1)
///
pub fn nil_error(result: Result(a, e)) -> Result(a, Nil) {
  map_error(result, fn(_) { Nil })
}

/// Returns the first value if it is Ok, otherwise return the second value.
///
/// ## Examples
///
///    > or(Ok(1), Ok(2))
///    Ok(1)
///
///    > or(Ok(1), Error("Error 2"))
///    Ok(1)
///
///    > or(Error("Error 1"), Ok(2))
///    Ok(2)
///
///    > or(Error("Error 1"), Error("Error 2"))
///    Error("Error 2")
///
pub fn or(first: Result(a, e), second: Result(a, e)) -> Result(a, e) {
  case first {
    Ok(_) -> first
    Error(_) -> second
  }
}

/// Returns the first value if it is Ok, otherwise evaluates the given function for a fallback value.
///
/// ## Examples
///
///    > lazy_or(Ok(1), fn() { Ok(2) })
///    Ok(1)
///
///    > lazy_or(Ok(1), fn() { Error("Error 2") })
///    Ok(1)
///
///    > lazy_or(Error("Error 1"), fn() { Ok(2) })
///    Ok(2)
///
///    > lazy_or(Error("Error 1"), fn() { Error("Error 2") })
///    Error("Error 2")
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
/// If all elements in the list are Ok then returns an Ok holding the list of values.
/// If any element is Error then returns the first error.
///
/// ## Examples
///    > all([Ok(1), Ok(2)])
///    Ok([1, 2])
///
///    > all([Ok(1), Error("e")])
///    Error("e")
pub fn all(results: List(Result(a, e))) -> Result(List(a), e) {
  list.try_map(results, fn(x) { x })
}

pub fn replace_error(result: Result(a, e1), error: e2) -> Result(a, e2) {
  result
  |> map_error(fn(_) { error })
}

/// Given a list of results
/// Return only the values inside Ok
///
/// ## Examples
///
/// ```
/// > values([Ok(1), None, Ok(3)])
/// [1, 3]
/// ```
///
pub fn values(results: List(Result(a, e))) -> List(a) {
  list.filter_map(results, fn(r) { r })
}
