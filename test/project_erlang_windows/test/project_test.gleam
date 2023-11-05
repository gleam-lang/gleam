import gleam/io
import gleam/dynamic.{type Dynamic}
import project
import gleeunit
import gleam/erlang/atom.{from_string as atom_from_string}

pub fn main() {
  project.main()
  io.println("Hello, from the Gleam test module!")
  io.println(erlang_function())
  io.println(elixir_function())
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

@external(erlang, "erlang_test_file", "main")
fn erlang_function() -> String

@external(erlang, "certifi", "cacertfile")
fn rebar3_dep_function() -> Dynamic

@external(erlang, "Elixir.ElixirTestFile", "main")
fn elixir_function() -> String

@external(erlang, "Elixir.Countries", "all")
fn mix_dep_function() -> Dynamic

// Testing for this bug in metadata encoding.
// https://github.com/gleam-lang/gleam/commit/c8f3bd0ddbf61c27ea35f37297058ecca7515f6c
pub fn name_test() {
  let assert True = atom.from_string("ok") == atom_from_string("ok")
}
