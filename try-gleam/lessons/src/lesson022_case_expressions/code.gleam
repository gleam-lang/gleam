import gleam/io
import gleam/int

pub fn main() {
  let result = case int.random(0, 5) {
    0 -> "It's zero!"
    other -> "It's " <> int.to_string(other)
  }
  io.debug(result)
}
