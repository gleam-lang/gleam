import std/string
import std/expect

pub fn length_test() {
  string:length("ß↑e̊")
  |> expect:equal(_, 3)

  string:length("Gleam")
  |> expect:equal(_, 5)

  string:length("")
  |> expect:equal(_, 0)
}

pub fn lowercase_test() {
  string:lowercase("Gleam")
  |> expect:equal(_, "gleam")
}

pub fn uppercase_test() {
  string:uppercase("Gleam")
  |> expect:equal(_, "GLEAM")
}

pub fn reverse_test() {
  string:reverse("Gleam")
  |> expect:equal(_, "maelG")
}

pub fn split_test() {
  "Gleam,Erlang,Elixir"
  |> string:split(_, ",")
  |> expect:equal(_, ["Gleam", "Erlang", "Elixir"])

  "Gleam, Erlang,Elixir"
  |> string:split(_, ", ")
  |> expect:equal(_, ["Gleam", "Erlang,Elixir"])
}

pub fn replace_test() {
  "Gleam,Erlang,Elixir"
  |> string:replace(_, ",", "++")
  |> expect:equal(_, "Gleam++Erlang++Elixir")
}

pub fn append_test() {
  "Test"
  |> string:append(_, " Me")
  |> expect:equal(_, "Test Me")
}
