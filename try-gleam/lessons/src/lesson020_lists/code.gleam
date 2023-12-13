import gleam/io

pub fn main() {
  let ints = [1, 2, 3]

  io.debug(ints)

  // Immutably prepend
  io.debug([-1, 0, ..ints])

  // Uncomment this to see the error
  // io.debug(["zero", ..ints])

  // The original lists are unchanged
  io.debug(ints)
}
