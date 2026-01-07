import gleam/io
import gleam/string

pub fn main() {
  let x = "ABC"
  case True {
    True -> {
      let x = 79
      0
    }
    False -> {
      let x = True
      0
    }
  }
  io.println(string.reverse(x))
}
