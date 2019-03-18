// Named str to avoid name collisions with the Erlang string module.
// Will rename later once we have namespaces for modules.

import expect
import iodata
import list

pub external fn length(String) -> Int = "string" "length"

test length {
  length("ß↑e̊")
  |> expect:equal(_, 3)

  length("Gleam")
  |> expect:equal(_, 5)

  length("")
  |> expect:equal(_, 0)

  // TODO: This crashes.
  // length("é")
  // |> expect:equal(_, 1)
}

pub external fn lowercase(String) -> String = "string" "lowercase"

test lowercase {
  lowercase("Gleam")
  |> expect:equal(_, "gleam")
}

pub external fn uppercase(String) -> String = "string" "uppercase"

test uppercase {
  uppercase("Gleam")
  |> expect:equal(_, "GLEAM")
}

pub fn reverse(string) {
  string
  |> iodata:new
  |> iodata:reverse
  |> iodata:to_string
}

test reverse {
  reverse("Gleam")
  |> expect:equal(_, "maelG")
}

pub fn split(string, on) {
  string
  |> iodata:new
  |> iodata:split(_, on)
  |> list:map(_, iodata:to_string)
}

test split {
  "Gleam,Erlang,Elixir"
  |> split(_, ",")
  |> expect:equal(_, ["Gleam", "Erlang", "Elixir"])

  "Gleam, Erlang,Elixir"
  |> split(_, ", ")
  |> expect:equal(_, ["Gleam", "Erlang,Elixir"])
}

pub fn replace(string, pattern, with) {
  string
  |> iodata:new
  |> iodata:replace(_, pattern, with)
  |> iodata:to_string
}

test replace {
  "Gleam,Erlang,Elixir"
  |> replace(_, ",", "++")
  |> expect:equal(_, "Gleam++Erlang++Elixir")
}
