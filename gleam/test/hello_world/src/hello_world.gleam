pub enum Result(error, value) =
  | Ok(value)
  | Error(error)

pub enum Size =
  | Big
  | Small

pub fn ok(x) {
  Ok(x)
}

pub fn error(x) {
  Error(x)
}

pub fn big() {
  Big
}

pub fn small() {
  Small
}

pub fn to_int(x) {
  case x {
  | Big -> 2
  | Small -> 1
  }
}
