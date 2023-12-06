import gleam/io
import gleam/int

pub fn main() {
  // Int arithmetic
  io.debug(1 + 1)
  io.debug(5 - 1)
  io.debug(5 / 2)
  io.debug(3 * 3)
  io.debug(5 % 2)

  // Int comparisons
  io.debug(2 > 1)
  io.debug(2 < 1)
  io.debug(2 >= 1)
  io.debug(2 <= 1)

  // Standard library int functions
  io.debug(int.max(42, 77))
  io.debug(int.clamp(5, 10, 20))
}
