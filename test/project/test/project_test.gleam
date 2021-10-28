import gleam/io
import project
import first_gleam_publish_package

pub fn main() {
  project.main()
  io.println("Hello, from the Gleam test module!")
  io.println(erlang_function())
  first_gleam_publish_package.main()
}

external fn erlang_function() -> String =
  "erlang_test_file" "main"
