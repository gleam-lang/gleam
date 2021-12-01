import gleam/should
import gleam/string_builder

pub fn string_builder_test() {
  let data =
    string_builder.from_string("ello")
    |> string_builder.append(",")
    |> string_builder.append(" world!")
    |> string_builder.prepend("H")

  data
  |> string_builder.to_string
  |> should.equal("Hello, world!")

  data
  |> string_builder.byte_size
  |> should.equal(13)

  let data =
    string_builder.from_string("ello")
    |> string_builder.append_builder(string_builder.from_string(","))
    |> string_builder.append_builder(string_builder.concat([
      string_builder.from_string(" wo"),
      string_builder.from_string("rld!"),
    ]))
    |> string_builder.prepend_builder(string_builder.from_string("H"))

  data
  |> string_builder.to_string
  |> should.equal("Hello, world!")

  data
  |> string_builder.byte_size
  |> should.equal(13)
}

pub fn lowercase_test() {
  ["Gleam", "Gleam"]
  |> string_builder.from_strings
  |> string_builder.lowercase
  |> string_builder.to_string
  |> should.equal("gleamgleam")
}

pub fn uppercase_test() {
  ["Gleam", "Gleam"]
  |> string_builder.from_strings
  |> string_builder.uppercase
  |> string_builder.to_string
  |> should.equal("GLEAMGLEAM")
}

pub fn split_test() {
  "Gleam,Erlang,Elixir"
  |> string_builder.from_string
  |> string_builder.split(",")
  |> should.equal([
    string_builder.from_string("Gleam"),
    string_builder.from_string("Erlang"),
    string_builder.from_string("Elixir"),
  ])

  ["Gleam, Erl", "ang,Elixir"]
  |> string_builder.from_strings
  |> string_builder.split(", ")
  |> should.equal([
    string_builder.from_string("Gleam"),
    string_builder.from_strings(["Erl", "ang,Elixir"]),
  ])
}

pub fn is_equal_test() {
  string_builder.from_string("12")
  |> string_builder.is_equal(string_builder.from_strings(["1", "2"]))
  |> should.be_true

  string_builder.from_string("12")
  |> string_builder.is_equal(string_builder.from_string("12"))
  |> should.be_true

  string_builder.from_string("12")
  |> string_builder.is_equal(string_builder.from_string("2"))
  |> should.be_false
}

pub fn is_empty_test() {
  string_builder.from_string("")
  |> string_builder.is_empty
  |> should.be_true

  string_builder.from_string("12")
  |> string_builder.is_empty
  |> should.be_false

  string_builder.from_strings([])
  |> string_builder.is_empty
  |> should.be_true

  string_builder.from_strings(["", ""])
  |> string_builder.is_empty
  |> should.be_true
}
