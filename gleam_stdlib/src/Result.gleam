module Result
  exposing Result(..), is_ok/1, map/2, map_error/2, flatten/1,
    flat_map/2, unwrap/2, to_maybe/1, from_maybe/1

import Maybe exposing Maybe(..)

doc """
Result represents the result of something that may succeed or fail.
`Ok` means it was successful, `Error` means it failed.
"""
type Result(error, value) {
  | Ok(value)
  | Error(error)

; // Fix GitHub syntax highlighting

fn is_ok(result) {
  case result {
  | Error(_) => False
  | Ok(_) => True
  }
}

test is_ok {
  is_ok(Ok(1)) |> Assert.true
  is_ok(Error(1)) |> Assert.false
}

fn is_error(result) {
  case result {
  | Ok(_) => False
  | Error(_) => True
  }
}

test is_error {
  is_error(Ok(1)) |> Assert.false
  is_error(Error(1)) |> Assert.true
}

fn map(result, fun) {
  case result {
  | Ok(x) => fun(x)
  | Error(_) => result
  }
}

test map {
  map(Ok(1), |x| x + 1) |> Assert.equal(_, Ok(2))
  map(Error(1), |x| x + 1) |> Assert.equal(Error(1))
}

fn map_error(result, fun) {
  case result {
  | Ok(_) => result
  | Error(error) => error |> fun |> Error
  }
}

test map_error {
  map_error(Ok(1), |x| x + 1) |> Assert.equal(_, Ok(1))
  map_error(Error(1), |x| x + 1) |> Assert.equal(Error(2))
}

fn flatten(result) {
  case result {
  | Ok(x) => x
  | Error(_) => result
  }
}

test flatten {
  flatten(Ok(Ok(1))) |> Assert.equal(_, Ok(1))
  flatten(Ok(Error(1))) |> Assert.equal(_, Error(1))
  flatten(Error(1)) |> Assert.equal(_, Error(1))
}

fn flat_map(result, fun) {
  result
    |> unwrap(_, fun)
    |> flatten
}

test flat_map {
  flat_map(Error(1), |x| Ok(x + 1)) |> Assert.equal(_, Error(1))
  flat_map(Ok(1), |x| Ok(x + 1)) |> Assert.equal(_, Ok(2))
  flat_map(Ok(1), |_| Error(1)) |> Assert.equal(_, Error(1))
}

fn unwrap(result, default) {
  case result {
  | Ok(v) => v
  | Error(_) => default
  }
}

test unwrap {
  unwrap(Ok(1), 50) |> Assert.equal(_, 1)
  unwrap(Error("nope"), 50) |> Assert.equal(_, 50)
}

fn to_maybe(result) {
  case result {
  | Ok(v) => Just(v)
  | Error(_) => Nothing
  }
}

test to_maybe {
  to_maybe(Ok(1)) |> Assert.equal(_, Just(_, 1))
  to_maybe(Error(1)) |> Assert.equal(_, Nothing)
}

fn from_maybe(maybe, error_reason) {
  case maybe {
  | Just(v) => Ok(v)
  | Nothing => Error(error_reason)
  }
}

test from_maybe {
  to_maybe(Just(1), :ok) |> Assert.equal(_, Ok(1))
  to_maybe(Nothing, :ok) |> Assert.equal(_, Error(:ok))
}
