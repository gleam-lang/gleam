import gleam/io

pub fn main() {
  // Call a function with another function
  io.debug(twice(1, add_one))

  // Functions can be assigned to variables
  let function = add_one
  io.debug(function(100))
}

fn twice(argument: Int, function: fn(Int) -> Int) -> Int {
  function(function(argument))
}

fn add_one(argument: Int) -> Int {
  argument + 1
}
