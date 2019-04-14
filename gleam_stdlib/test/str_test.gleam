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

pub fn from_int_test() {
  123
  |> str:from_int
  |> expect:equal(_, "123")

  -123
  |> str:from_int
  |> expect:equal(_, "-123")

  0123
  |> str:from_int
  |> expect:equal(_, "123")
}

pub fn parse_int_test() {
  "123"
  |> str:parse_int
  |> expect:equal(_, Ok(123))

  "-123"
  |> str:parse_int
  |> expect:equal(_, Ok(-123))

  "0123"
  |> str:parse_int
  |> expect:equal(_, Ok(123))

  ""
  |> str:parse_int
  |> expect:is_error

  "what"
  |> str:parse_int
  |> expect:is_error

  "1.23"
  |> str:parse_int
  |> expect:is_error
}

pub fn parse_float_test() {
  "1.23"
  |> str:parse_float
  |> expect:equal(_, Ok(1.23))

  "5.0"
  |> str:parse_float
  |> expect:equal(_, Ok(5.0))

  "0.123456789"
  |> str:parse_float
  |> expect:equal(_, Ok(0.123456789))

  ""
  |> str:parse_float
  |> expect:is_error

  "what"
  |> str:parse_float
  |> expect:is_error

  "1"
  |> str:parse_float
  |> expect:is_error
}

pub fn base_from_int_test() {
  100
  |> str:base_from_int(_, 16)
  |> expect:equal(_, "64")

  -100
  |> str:base_from_int(_, 16)
  |> expect:equal(_, "-64")
}

pub fn from_float_test() {
  123.0
  |> str:from_float
  |> expect:equal(_, "123.0")

  -8.1
  |> str:from_float
  |> expect:equal(_, "-8.1")
}
