import gleam/io
import gleam/list

pub fn main() {
  let ints = [0, 1, 2, 3, 4, 5]

  let doubled = list.map(ints, fn(x) { x * 2 })
  io.debug(doubled)

  let even = list.filter(ints, fn(x) { x % 2 == 0 })
  io.debug(even)

  let total = list.fold(ints, from: 0, with: fn(count, e) { count + e })
  io.debug(total)
}
