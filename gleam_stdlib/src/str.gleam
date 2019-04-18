// Named str to avoid name collisions with the Erlang string module.
// Will rename later once we have namespaces for modules.

import iodata
import list

pub external fn length(String) -> Int = "string" "length"

pub enum ParseError =
  | ParseError

pub external fn lowercase(String) -> String = "string" "lowercase"

pub external fn uppercase(String) -> String = "string" "uppercase"

pub fn reverse(string) {
  string
  |> iodata:new
  |> iodata:reverse
  |> iodata:to_string
}

pub fn split(string, on) {
  string
  |> iodata:new
  |> iodata:split(_, on)
  |> list:map(_, iodata:to_string)
}

pub fn replace(string, pattern, with) {
  string
  |> iodata:new
  |> iodata:replace(_, pattern, with)
  |> iodata:to_string
}

pub fn append(s1, s2) {
  iodata:new(s1) |> iodata:append(_, s2) |> iodata:to_string(_)
}

pub external fn from_int(Int) -> String = "erlang" "integer_to_binary"

pub external fn parse_int(String) -> Result(Int, ParseError) = "gleam__stdlib" "parse_int";

pub external fn parse_float(String) -> Result(Float, ParseError) = "gleam__stdlib" "parse_float";

pub external fn base_from_int(Int, Int) -> String = "erlang" "integer_to_binary"

pub fn from_float(f) {
  f
  |> iodata:from_float
  |> iodata:to_string
}
