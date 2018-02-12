module Result

export Result(..), ok?/1, none?/1, map/2, map_error/2, flatten/1, flat_map/2,
       unwrap/2, to_maybe/1, from_maybe/1

import Maybe exposing Maybe(..)


/// Result represents the result of something that may succeed or fail.
/// `Ok` means it was successful, `Error` means it failed.
///
type Result(error, value)
  = Ok(value)
  | Error(error)

let ok?(result) =
  match result
  | Error(_) => False
  | Ok(_) => True

test "ok?/1" =
  ok?(Ok(1)) |> Assert.true
  ok?(Error(1)) |> Assert.false

let error?(result) =
  match result
  | Ok(_) => False
  | Error(_) => True

test "error?/1" =
  error?(Ok(1)) |> Assert.false
  error?(Error(1)) |> Assert.true

let map(result, fun) =
  match result
  | Ok(x) => fun(x)
  | Error(_) => result

test "map/2" =
  map(Ok(1), |x| x + 1) |> Assert.equal(_, Ok(2))
  map(Error(1), |x| x + 1) |> Assert.equal(Error(1))

let map_error(result, fun) =
  match result
  | Ok(_) => result
  | Error(error) => error |> fun |> Error

test "map_error/2" =
  map_error(Ok(1), |x| x + 1) |> Assert.equal(_, Ok(1))
  map_error(Error(1), |x| x + 1) |> Assert.equal(Error(2))

let flatten(result) =
  match result
  | Ok(x) => x
  | Error(_) => result

test "flatten/1" =
  flatten(Ok(Ok(1))) |> Assert.equal(_, Ok(1))
  flatten(Ok(Error(1))) |> Assert.equal(_, Error(1))
  flatten(Error(1)) |> Assert.equal(_, Error(1))

let flat_map(result, fun) =
  result
    |> unwrap(_, fun)
    |> flatten

test "flat_map/2" =
  flat_map(Error(1), |x| Ok(x + 1)) |> Assert.equal(_, Error(1))
  flat_map(Ok(1), |x| Ok(x + 1)) |> Assert.equal(_, Ok(2))
  flat_map(Ok(1), |_| Error(1)) |> Assert.equal(_, Error(1))

let unwrap(result, default) =
  match result
  | Ok(v) => v
  | Error(_) => default

test "unwrap/2" =
  unwrap(Ok(1), 50) |> Assert.equal(_, 1)
  unwrap(Error("nope"), 50) |> Assert.equal(_, 50)

let to_maybe(result) =
  match result
  | Ok(v) => Just(v)
  | Error(_) => Nothing

test "to_maybe/1" =
  to_maybe(Ok(1)) |> Assert.equal(_, Just(_, 1))
  to_maybe(Error(1)) |> Assert.equal(_, Nothing)

let from_maybe(maybe, error_reason) =
  match maybe
  | Just(v) => Ok(v)
  | Nothing => Error(error_reason)

test "from_maybe/2" =
  to_maybe(Just(1), :ok) |> Assert.equal(_, Ok(1))
  to_maybe(Nothing, :ok) |> Assert.equal(_, Error(:ok))
