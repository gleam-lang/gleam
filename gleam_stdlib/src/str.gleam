// Named str to avoid name collisions with the Erlang string module.
// Will rename later once we have namespaces for modules.

import expect
import iodata
import list

pub external fn length(String) -> Int = "string" "length"

pub enum ParseError =
  | ParseError

test length {
  length("ß↑e̊")
  |> expect:equal(_, 3)

  length("Gleam")
  |> expect:equal(_, 5)

  length("")
  |> expect:equal(_, 0)
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

pub external fn from_int(Int) -> String = "erlang" "integer_to_binary"

test from_int {
  123
  |> from_int
  |> expect:equal(_, "123")

  -123
  |> from_int
  |> expect:equal(_, "-123")

  0123
  |> from_int
  |> expect:equal(_, "123")
}

pub external fn parse_int(String) -> Result(Int, ParseError) = "gleam__stdlib" "parse_int";

test parse_int {
  "123"
  |> parse_int
  |> expect:equal(_, Ok(123))

  "-123"
  |> parse_int
  |> expect:equal(_, Ok(-123))

  "0123"
  |> parse_int
  |> expect:equal(_, Ok(123))
}

pub external fn base_from_int(Int, Int) -> String = "erlang" "integer_to_binary"

test base_from_int {
  100
  |> base_from_int(_, 16)
  |> expect:equal(_, "64")

  -100
  |> base_from_int(_, 16)
  |> expect:equal(_, "-64")
}

pub fn from_float(f) {
  f
  |> iodata:from_float
  |> iodata:to_string
}

test from_float {
  123.0
  |> from_float
  |> expect:equal(_, "123.0")

  -8.1
  |> from_float
  |> expect:equal(_, "-8.1")
}
