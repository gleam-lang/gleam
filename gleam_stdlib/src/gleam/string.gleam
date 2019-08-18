import gleam/iodata
import gleam/list

pub external fn length(String) -> Int = "string" "length"

pub enum ParseError =
  | ParseError

pub external fn lowercase(String) -> String = "string" "lowercase"

pub external fn uppercase(String) -> String = "string" "uppercase"

pub fn reverse(string) {
  string
  |> iodata.new
  |> iodata.reverse
  |> iodata.to_string
}

pub fn split(string, on) {
  string
  |> iodata.new
  |> iodata.split(_, on)
  |> list.map(_, iodata.to_string)
}

pub fn replace(string, pattern, with) {
  string
  |> iodata.new
  |> iodata.replace(_, pattern, with)
  |> iodata.to_string
}

pub fn append(s1, s2) {
  iodata.new(s1) |> iodata.append(_, s2) |> iodata.to_string(_)
}
