import gleam/expect
import gleam/iodata

pub fn iodata_test() {
  let data = iodata:new("ello")
    |> iodata:append(_, ",")
    |> iodata:append(_, " world!")
    |> iodata:prepend(_, "H")

  data
  |> iodata:to_string
  |> expect:equal(_, "Hello, world!")

  data
  |> iodata:byte_size
  |> expect:equal(_, 13)

  let data = iodata:new("ello")
    |> iodata:append_iodata(_, iodata:new(","))
    |> iodata:append_iodata(_, iodata:concat([iodata:new(" wo"), iodata:new("rld!")]))
    |> iodata:prepend_iodata(_, iodata:new("H"))

  data
  |> iodata:to_string
  |> expect:equal(_, "Hello, world!")

  data
  |> iodata:byte_size
  |> expect:equal(_, 13)
}

pub fn lowercase_test() {
  ["Gleam", "Gleam"]
  |> iodata:from_strings
  |> iodata:lowercase
  |> iodata:to_string
  |> expect:equal(_, "gleamgleam")
}

pub fn uppercase_test() {
  ["Gleam", "Gleam"]
  |> iodata:from_strings
  |> iodata:uppercase
  |> iodata:to_string
  |> expect:equal(_, "GLEAMGLEAM")
}

pub fn split_test() {
  "Gleam,Erlang,Elixir"
  |> iodata:new
  |> iodata:split(_, ",")
  |> expect:equal(_, [iodata:new("Gleam"), iodata:new("Erlang"), iodata:new("Elixir")])

  ["Gleam, Erl", "ang,Elixir"]
  |> iodata:from_strings
  |> iodata:split(_, ", ")
  |> expect:equal(_, [iodata:new("Gleam"), iodata:from_strings(["Erl", "ang,Elixir"])])
}

pub fn is_equal_test() {
  iodata:new("12")
  |> iodata:is_equal(_, iodata:from_strings(["1", "2"]))
  |> expect:true

  iodata:new("12")
  |> iodata:is_equal(_, iodata:new("12"))
  |> expect:true

  iodata:new("12")
  |> iodata:is_equal(_, iodata:new("2"))
  |> expect:false
}

pub fn is_empty_test() {
  iodata:new("")
  |> iodata:is_empty
  |> expect:true

  iodata:new("12")
  |> iodata:is_empty
  |> expect:false

  iodata:from_strings([])
  |> iodata:is_empty
  |> expect:true

  iodata:from_strings(["", ""])
  |> iodata:is_empty
  |> expect:true
}
