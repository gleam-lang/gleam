import gleam/io
import gleam/dynamic.{Dynamic}
import project
import first_gleam_publish_package
import gleeunit
import gleam/erlang/atom.{from_string as atom_from_string}

pub fn main() {
  project.main()
  io.println("Hello, from the Gleam test module!")
  io.println(erlang_function())
  io.println(elixir_function())
  first_gleam_publish_package.main()
  gleeunit.main()
}

pub fn rebar3_dep_function_test() {
  io.println("Testing calling a rebar3 library (that uses headers)")
  rebar3_dep_function()
}

pub fn mix_dep_function_test() {
  io.println("Testing calling a mix library")
  mix_dep_function()
}

external fn erlang_function() -> String =
  "erlang_test_file" "main"

external fn rebar3_dep_function() -> Dynamic =
  "certifi" "cacertfile"

external fn elixir_function() -> String =
  "Elixir.ElixirTestFile" "main"

external fn mix_dep_function() -> Dynamic =
  "Elixir.Countries" "all"

// Testing for this bug in metadata encoding.
// https://github.com/gleam-lang/gleam/commit/c8f3bd0ddbf61c27ea35f37297058ecca7515f6c
pub fn name_test() {
  let assert True = atom.from_string("ok") == atom_from_string("ok")
}
