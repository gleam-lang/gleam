import gleam/io

pub fn main() {
  io.println("Hello, from Gleam compiled to Erlang!")
  io.println(erlang_function())
  io.println(elixir_function())
  io.println(another_elixir_function())
}

@external(erlang, "erlang_file", "main")
fn erlang_function() -> String

@external(erlang, "Elixir.ElixirFile", "main")
fn elixir_function() -> String

@external(erlang, "Elixir.ElixirFileAgain", "main")
fn another_elixir_function() -> String
