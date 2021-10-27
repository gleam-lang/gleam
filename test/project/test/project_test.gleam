import gleam/io
import project

pub fn main() {
  project.main()
  io.println("Hello, from the Gleam test module!")
  io.println(erlang_function())
}

external fn erlang_function() -> String =
  "erlang_test_file" "main"
