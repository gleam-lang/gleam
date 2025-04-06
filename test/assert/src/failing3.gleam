import gleam/bool
import gleam/result

pub fn main() {
  assert bool.negate(False) && result.is_ok(Error(81))
}
