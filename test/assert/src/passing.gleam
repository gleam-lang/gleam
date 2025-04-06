import gleam/bool
import gleam/int
import gleam/result

/// All these assertions should succeed
pub fn main() {
  let x = True
  assert x
  assert case x && False {
    True -> False
    False -> True
  }

  assert result.is_ok(Ok(10))
  assert result.is_error(Error("Hello"))

  assert int.add(5, 6) == 11
  assert int.add(1, 4) < 9
  assert int.add(8, 12) >= 20
  assert bool.negate(False) && result.is_ok(Ok(42))
  assert int.is_even(3) || int.is_even(4)

  // This should short-circuit so we don't panic here
  assert True || panic
}
