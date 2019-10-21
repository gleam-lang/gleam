import gleam/iodata
import gleam/list
import gleam/order

pub external fn length(String) -> Int = "string" "length"

pub external fn lowercase(String) -> String = "string" "lowercase"

pub external fn uppercase(String) -> String = "string" "uppercase"

pub external fn compare(String, String) -> order.Order =
  "gleam_stdlib" "compare_strings"

pub fn reverse(string) {
  string
  |> iodata.new
  |> iodata.reverse
  |> iodata.to_string
}

pub fn split(string x, on pattern) {
  x
  |> iodata.new
  |> iodata.split(_, on: pattern)
  |> list.map(_, with: iodata.to_string)
}

pub fn replace(in string, all pattern, with substitute) {
  string
  |> iodata.new
  |> iodata.replace(_, all: pattern, with: substitute)
  |> iodata.to_string
}

pub fn append(to first, suffix second) {
  first
  |> iodata.new
  |> iodata.append(_, second)
  |> iodata.to_string
}
