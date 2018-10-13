import assert

doc """
Result represents the result of something that may succeed or fail.
`Ok` means it was successful, `Error` means it failed.
"""
pub enum Result(error, value) =
  | Ok(value)
  | Error(error)
;

pub fn is_ok(result) {
  case result {
  | Error(_) => False
  | Ok(_) => True
  }
}

test is_ok {
  is_ok(Ok(1)) |> assert:true
  is_ok(Error(1)) |> assert:false
}

pub fn is_error(result) {
  case result {
  | Ok(_) => False
  | Error(_) => True
  }
}

test is_error {
  is_error(Ok(1)) |> assert:false
  is_error(Error(1)) |> assert:true
}

pub fn map(result, fun) {
  case result {
  | Ok(x) => fun(x)
  | Error(_) => result
  }
}

test map {
  Ok(1)
    |> map(_, fn(x) { x + 1 })
    |> assert:equal(_, Ok(2))
  Error(1)
    |> map(_, fn(x) { x + 1 })
    |> assert:equal(Error(1))
}

pub fn map_error(result, fun) {
  case result {
  | Ok(_) => result
  | Error(error) => Error(fun(error))
  }
}

test map_error {
  Ok(1)
    |> map_error(_, fn(x) { x + 1 })
    |> assert:equal(_, Ok(1))
  Error(1)
    |> map_error(_, fn(x) { x + 1 })
    |> assert:equal(_, Error(2))
}

pub fn flatten(result) {
  case result {
  | Ok(x) => x
  | Error(_) => result
  }
}

test flatten {
  flatten(Ok(Ok(1)))
    |> assert:equal(_, Ok(1))
  flatten(Ok(Error(1)))
    |> assert:equal(_, Error(1))
  flatten(Error(1))
    |> assert:equal(_, Error(1))
}

pub fn flat_map(result, fun) {
  result
    |> unwrap(_, fun)
    |> flatten
}

test flat_map {
  Error(1)
    |> flat_map(_, fn(x) { Ok(x + 1) })
    |> assert:equal(_, Error(1))
  Ok(1)
    |> flat_map(_, fn(x) { Ok(x + 1) })
    |> assert:equal(_, Ok(2))
  Ok(1)
    |> flat_map(_, fn(_) { Error(1) })
    |> assert:equal(_, Error(1))
}

pub fn unwrap(result, default) {
  case result {
  | Ok(v) => v
  | Error(_) => default
  }
}

test unwrap {
  unwrap(Ok(1), 50) |> assert:equal(_, 1)
  unwrap(Error("nope"), 50) |> assert:equal(_, 50)
}
