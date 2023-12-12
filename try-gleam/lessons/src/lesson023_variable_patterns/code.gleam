import gleam/io
import gleam/int

pub fn main() {
  let x = int.random(0, 5)
  io.debug(x)

  let result = case x {
    // Match specific values
    0 -> "Zero"
    1 -> "One"
    // Match any other value
    _ -> "Other"
  }
  io.debug(result)
}
