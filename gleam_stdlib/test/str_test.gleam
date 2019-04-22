import str
import expect

pub fn length_test() {
  str:length("ß↑e̊")
  |> expect:equal(_, 3)

  str:length("Gleam")
  |> expect:equal(_, 5)

  str:length("")
  |> expect:equal(_, 0)
}

pub fn lowercase_test() {
  str:lowercase("Gleam")
  |> expect:equal(_, "gleam")
}

pub fn uppercase_test() {
  str:uppercase("Gleam")
  |> expect:equal(_, "GLEAM")
}

pub fn reverse_test() {
  str:reverse("Gleam")
  |> expect:equal(_, "maelG")
}

pub fn split_test() {
  "Gleam,Erlang,Elixir"
  |> str:split(_, ",")
  |> expect:equal(_, ["Gleam", "Erlang", "Elixir"])

  "Gleam, Erlang,Elixir"
  |> str:split(_, ", ")
  |> expect:equal(_, ["Gleam", "Erlang,Elixir"])
}

pub fn replace_test() {
  "Gleam,Erlang,Elixir"
  |> str:replace(_, ",", "++")
  |> expect:equal(_, "Gleam++Erlang++Elixir")
}

pub fn append_test() {
  str:append("Test", " Me")
  |> expect:equal(_, "Test Me")
}
