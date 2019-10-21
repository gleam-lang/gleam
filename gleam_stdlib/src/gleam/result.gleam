// Result represents the result of something that may succeed or fail.
// `Ok` means it was successful, `Error` means it failed.

pub fn is_ok(result) {
  case result {
  | Error(_) -> False
  | Ok(_) -> True
  }
}

pub fn is_error(result) {
  case result {
  | Ok(_) -> False
  | Error(_) -> True
  }
}

pub fn map(result, with fun) {
  case result {
  | Ok(x) -> Ok(fun(x))
  | Error(e) -> Error(e)
  }
}

pub fn map_error(result, with fun) {
  case result {
  | Ok(x) -> Ok(x)
  | Error(error) -> Error(fun(error))
  }
}

pub fn flatten(result) {
  case result {
  | Ok(x) -> x
  | Error(error) -> Error(error)
  }
}

pub fn then(result, apply fun) {
  case result {
  | Ok(x) -> fun(x)
  | Error(e) -> Error(e)
  }
}

pub fn unwrap(result, or default) {
  case result {
  | Ok(v) -> v
  | Error(_) -> default
  }
}
