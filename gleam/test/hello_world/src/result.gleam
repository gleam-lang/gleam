pub enum Bool =
  | True
  | False

pub enum Result(error, value) =
  | Ok(value)
  | Error(error)

pub fn is_ok(result) {
  case result {
  | Error(a) -> False
  | Ok(a) -> True
  }
}

pub fn is_error(result) {
  case result {
  | Ok(a) -> False
  | Error(a) -> True
  }
}

pub fn map(result, fun) {
  case result {
  | Ok(x) -> Ok(fun(x))
  | Error(x) -> result
  }
}

pub fn map_error(result, fun) {
  case result {
  | Ok(a) -> result
  | Error(error) -> Error(fun(error))
  }
}

pub fn flatten(result) {
  case result {
  | Ok(x) -> x
  | Error(error) -> Error(error)
  }
}

pub fn flat_map(result, fun) {
  flatten(map(result, fun))
}

pub fn unwrap(result, default) {
  case result {
  | Ok(v) -> v
  | Error(a) -> default
  }
}

