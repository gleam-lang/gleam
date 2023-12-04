// Import the module and one of its functions
import gleam/io.{println}

pub fn main() {
  // Use the function in a qualified fashion
  io.println("This is qualified")

  // Or an unqualified fashion
  println("This is unqualified")
}
