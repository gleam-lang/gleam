// This should raise a warning as `gleam_stdlib` is a dev dependency.
import gleam/io

pub fn main() {
  io.println("Hello, world!")
}
