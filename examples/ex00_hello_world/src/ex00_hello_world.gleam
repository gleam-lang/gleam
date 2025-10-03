import gleam/int
import gleam/io

// This is a simple Gleam program that prints the sum of two numbers.

pub fn main() -> Nil {
  let sum = add(2, 3)
  io.println("The sum of 2 and 3 is:")
  io.println(int.to_string(sum))
  Nil
}

/// Adds two integers and returns the result.
/// ## Parameters
/// - `a`: The first integer.
/// - `b`: The second integer.
/// ## Returns
/// The sum of `a` and `b`.
/// ## Examples
/// ```gleam
/// let result = add(2, 3)
/// // result is 5
/// ```
pub fn add(a: Int, b: Int) -> Int {
  a + b
}
