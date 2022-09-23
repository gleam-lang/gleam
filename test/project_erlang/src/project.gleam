import gleam/io

pub fn main() {
  io.println("Hello, from Gleam compiled to Erlang!")
  io.println(erlang_function())
  io.println(elixir_function())
  io.println(another_elixir_function())
}

external fn erlang_function() -> String =
  "erlang_file" "main"

external fn elixir_function() -> String =
  "Elixir.ElixirFile" "main"

external fn another_elixir_function() -> String =
  "Elixir.ElixirFileAgain" "main"
