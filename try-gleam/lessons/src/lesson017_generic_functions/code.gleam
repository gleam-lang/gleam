import gleam/io

pub fn main() {
  let add_one = fn(x) { x + 1 }
  let exclaim = fn(x) { x <> "!" }

  // Invalid, Int and String are not the same type
  // twice(10, exclaim)

  // Here the type variable is replaced by the type Int
  io.debug(twice(10, add_one))

  // Here the type variable is replaced by the type String
  io.debug(twice("Hello", exclaim))
}

fn twice(argument: value, function: fn(value) -> value) -> value {
  function(function(argument))
}
