import gleam

pub fn gleam_ok_test() {
  let left = Ok(1)
  let right = gleam.Ok(1)
  assert left == right
}

pub fn gleam_error_test() {
  let left = Error(1)
  let right = gleam.Error(1)
  assert left == right
}

pub fn gleam_nil_test() {
  let left = Nil
  let right = gleam.Nil
  assert left == right
}
