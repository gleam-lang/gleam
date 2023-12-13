import gleam/io

pub fn main() {
  // These two statements are equivalent
  let add_one_v1 = fn(x) { add(1, x) }
  let add_one_v2 = add(1, _)

  io.debug(add_one_v1(10))
  io.debug(add_one_v2(10))
}

fn add(a: Int, b: Int) -> Int {
  a + b
}
