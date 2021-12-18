import gleam/io

pub fn main() {
  io.println("Hello, from the Gleam module!")
  io.println(erlang_function())
}

external fn erlang_function() -> String =
  "erlang_file" "main"
