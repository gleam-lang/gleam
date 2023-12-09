import gleam/io

pub fn main() {
  // Assign an anonymous function to a variable
  let add_one = fn(a) { a + 1 }
  io.debug(twice(1, add_one))

  // Pass an anonymous function as an argument
  io.debug(twice(1, fn(a) { a * 2 }))
}

fn twice(argument: Int, function: fn(Int) -> Int) -> Int {
  function(function(argument))
}
