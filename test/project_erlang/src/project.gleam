import gleam/io

pub fn main() {
  io.println("Hello, from Gleam compiled to Erlang!")
  io.println(erlang_function())
}

external fn erlang_function() -> String =
  "erlang_file" "main"
