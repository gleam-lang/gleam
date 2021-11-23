import gleam/io
import gleam/dynamic.{Dynamic}
import project
import first_gleam_publish_package
import gleeunit

pub fn main() {
  project.main()
  io.println("Hello, from the Gleam test module!")
  io.println(erlang_function())
  first_gleam_publish_package.main()
  gleeunit.main()
}

pub fn rebar3_dep_function_test() {
  io.println("Testing calling a rebar3 library (that uses headers)")
  assert [] = rebar3_dep_function("")
}

external fn erlang_function() -> String =
  "erlang_test_file" "main"

external fn rebar3_dep_function(String) -> List(Dynamic) =
  "accept_header" "parse"
