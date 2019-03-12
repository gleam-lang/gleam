import expect

// Result represents the result of something that may succeed or fail.
// `Ok` means it was successful, `Error` means it failed.
//
pub enum Result(error, value) =
  | Ok(value)
  | Error(error)
;

pub fn is_ok(result) {
  case result {
  | Error(_) -> False
  | Ok(_) -> True
  }
}

test is_ok {
  is_ok(Ok(1)) |> expect:true
  is_ok(Error(1)) |> expect:false
}

pub fn is_error(result) {
  case result {
  | Ok(_) -> False
  | Error(_) -> True
  }
}

test is_error {
  is_error(Ok(1))
    |> expect:false

  is_error(Error(1))
    |> expect:true
}

pub fn map(result, fun) {
  case result {
  | Ok(x) -> Ok(fun(x))
  | Error(_) -> result
  }
}

test map {
  Ok(1)
    |> map(_, fn(x) { x + 1 })
    |> expect:equal(_, Ok(2))

  Error(1)
    |> map(_, fn(x) { x + 1 })
    |> expect:equal(_, Error(1))
}

pub fn map_error(result, fun) {
  case result {
  | Ok(_) -> result
  | Error(error) -> Error(fun(error))
  }
}

test map_error {
  Ok(1)
    |> map_error(_, fn(x) { x + 1 })
    |> expect:equal(_, Ok(1))

  Error(1)
    |> map_error(_, fn(x) { x + 1 })
    |> expect:equal(_, Error(2))
}

pub fn flatten(result) {
  case result {
  | Ok(x) -> x
  | Error(error) -> Error(error)
  }
}

test flatten {
  flatten(Ok(Ok(1)))
    |> expect:equal(_, Ok(1))

  flatten(Ok(Error(1)))
    |> expect:equal(_, Error(1))

  flatten(Error(1))
    |> expect:equal(_, Error(1))

  flatten(Error(Error(1)))
    |> expect:equal(_, Error(Error(1)))
}

pub fn flat_map(result, fun) {
  case result {
  | Ok(x) ->
      case fun(x) {
      | Ok(y) -> Ok(y)
      | Error(y) -> Error(y)
      }
  | Error(_) -> result
  }
}

test flat_map {
  Error(1)
    |> flat_map(_, fn(x) { Ok(x + 1) })
    |> expect:equal(_, Error(1))

  Ok(1)
    |> flat_map(_, fn(x) { Ok(x + 1) })
    |> expect:equal(_, Ok(2))

  Ok(1)
    |> flat_map(_, fn(unused) { Error(1) })
    |> expect:equal(_, Error(1))
}

pub fn unwrap(result, default) {
  case result {
  | Ok(v) -> v
  | Error(_) -> default
  }
}

test unwrap {
  unwrap(Ok(1), 50)
    |> expect:equal(_, 1)

  unwrap(Error("nope"), 50)
    |> expect:equal(_, 50)
}
