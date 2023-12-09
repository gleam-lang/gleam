import gleam/io

pub fn main() {
  // Call a function with another function
  io.debug(twice(1, add_one))

  // Functions can be assigned to variables
  let function = add_one
  io.debug(function(100))
}

fn twice(a: Int, function: fn(Int) -> Int) -> Int {
  function(function(a))
}

fn add_one(a: Int) -> Int {
  a + 1
}
