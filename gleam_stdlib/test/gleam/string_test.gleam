import gleam/string
import gleam/expect
import gleam/order

pub fn length_test() {
  string.length("ß↑e̊")
  |> expect.equal(_, 3)

  string.length("Gleam")
  |> expect.equal(_, 5)

  string.length("")
  |> expect.equal(_, 0)
}

pub fn lowercase_test() {
  string.lowercase("Gleam")
  |> expect.equal(_, "gleam")
}

pub fn uppercase_test() {
  string.uppercase("Gleam")
  |> expect.equal(_, "GLEAM")
}

pub fn reverse_test() {
  string.reverse("Gleam")
  |> expect.equal(_, "maelG")
}

pub fn split_test() {
  "Gleam,Erlang,Elixir"
  |> string.split(_, ",")
  |> expect.equal(_, ["Gleam", "Erlang", "Elixir"])

  "Gleam, Erlang,Elixir"
  |> string.split(_, ", ")
  |> expect.equal(_, ["Gleam", "Erlang,Elixir"])
}

pub fn replace_test() {
  "Gleam,Erlang,Elixir"
  |> string.replace(_, ",", "++")
  |> expect.equal(_, "Gleam++Erlang++Elixir")
}

pub fn append_test() {
  "Test"
  |> string.append(_, " Me")
  |> expect.equal(_, "Test Me")
}

pub fn compare_test() {
  string.compare("", "")
  |> expect.equal(_, order.Eq)

  string.compare("a", "")
  |> expect.equal(_, order.Gt)

  string.compare("a", "A")
  |> expect.equal(_, order.Gt)

  string.compare("A", "B")
  |> expect.equal(_, order.Lt)

  string.compare("t", "ABC")
  |> expect.equal(_, order.Gt)
}
