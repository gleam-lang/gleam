// https://github.com/gleam-lang/gleam/issues/1991
pub fn block_scoping_test() {
  let x = 1
  let _ = {
    let x = 2
    x
  }
  assert x == 1
}
