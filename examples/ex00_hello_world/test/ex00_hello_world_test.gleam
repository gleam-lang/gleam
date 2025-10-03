import ex00_hello_world
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn add_test() {
  let a = 2
  let b = 3
  let result = ex00_hello_world.add(a, b)
  assert result == 5
}
