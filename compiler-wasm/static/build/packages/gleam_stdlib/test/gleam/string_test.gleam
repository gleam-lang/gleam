import gleam/string
import gleam/should
import gleam/order
import gleam/option.{None, Some}
import gleam/io
import gleam/bit_string

pub fn length_test() {
  string.length("ß↑e̊")
  |> should.equal(3)

  string.length("Gleam")
  |> should.equal(5)

  string.length("")
  |> should.equal(0)
}

pub fn lowercase_test() {
  string.lowercase("Gleam")
  |> should.equal("gleam")
}

pub fn uppercase_test() {
  string.uppercase("Gleam")
  |> should.equal("GLEAM")
}

pub fn reverse_test() {
  string.reverse("Gleam")
  |> should.equal("maelG")
}

pub fn unicode_reverse_test() {
  string.reverse("👍 OK")
  |> should.equal("KO 👍")
}

pub fn split_test() {
  "Gleam,Erlang,Elixir"
  |> string.split(",")
  |> should.equal(["Gleam", "Erlang", "Elixir"])

  "Gleam, Erlang,Elixir"
  |> string.split(", ")
  |> should.equal(["Gleam", "Erlang,Elixir"])
}

pub fn split_once_test() {
  "Gleam,Erlang,Elixir"
  |> string.split_once(",")
  |> should.equal(Ok(#("Gleam", "Erlang,Elixir")))

  "Gleam"
  |> string.split_once(",")
  |> should.equal(Error(Nil))

  ""
  |> string.split_once(",")
  |> should.equal(Error(Nil))
}

pub fn replace_test() {
  "Gleam,Erlang,Elixir"
  |> string.replace(",", "++")
  |> should.equal("Gleam++Erlang++Elixir")
}

pub fn append_test() {
  "Test"
  |> string.append(" Me")
  |> should.equal("Test Me")
}

pub fn compare_test() {
  string.compare("", "")
  |> should.equal(order.Eq)

  string.compare("a", "")
  |> should.equal(order.Gt)

  string.compare("a", "A")
  |> should.equal(order.Gt)

  string.compare("A", "B")
  |> should.equal(order.Lt)

  string.compare("t", "ABC")
  |> should.equal(order.Gt)
}

pub fn contains_test() {
  "gleam"
  |> string.contains("ea")
  |> should.equal(True)

  "gleam"
  |> string.contains("x")
  |> should.equal(False)

  string.contains(does: "bellwether", contain: "bell")
  |> should.equal(True)
}

pub fn concat_test() {
  ["Hello", ", ", "world!"]
  |> string.concat
  |> should.equal("Hello, world!")
}

pub fn repeat_test() {
  "hi"
  |> string.repeat(times: 3)
  |> should.equal("hihihi")

  "hi"
  |> string.repeat(0)
  |> should.equal("")

  "hi"
  |> string.repeat(-1)
  |> should.equal("")
}

pub fn join_test() {
  ["Hello", "world!"]
  |> string.join(with: ", ")
  |> should.equal("Hello, world!")

  ["Hello", "world!"]
  |> string.join(with: "-")
  |> should.equal("Hello-world!")
}

pub fn trim_test() {
  "  hats  \n"
  |> string.trim()
  |> should.equal("hats")
}

pub fn trim_left_test() {
  "  hats  \n"
  |> string.trim_left()
  |> should.equal("hats  \n")
}

pub fn trim_right_test() {
  "  hats  \n"
  |> string.trim_right()
  |> should.equal("  hats")
}

pub fn starts_with_test() {
  "theory"
  |> string.starts_with("")
  |> should.equal(True)

  "theory"
  |> string.starts_with("the")
  |> should.equal(True)

  "theory"
  |> string.starts_with("ory")
  |> should.equal(False)

  "theory"
  |> string.starts_with("theory2")
  |> should.equal(False)
}

pub fn ends_with_test() {
  "theory"
  |> string.ends_with("")
  |> should.equal(True)

  "theory"
  |> string.ends_with("ory")
  |> should.equal(True)

  "theory"
  |> string.ends_with("the")
  |> should.equal(False)

  "theory"
  |> string.ends_with("theory2")
  |> should.equal(False)
}

pub fn slice_test() {
  "gleam"
  |> string.slice(at_index: 1, length: 2)
  |> should.equal("le")

  "gleam"
  |> string.slice(at_index: 1, length: 10)
  |> should.equal("leam")

  "gleam"
  |> string.slice(at_index: 10, length: 3)
  |> should.equal("")

  "gleam"
  |> string.slice(at_index: -2, length: 2)
  |> should.equal("am")

  "gleam"
  |> string.slice(at_index: -12, length: 2)
  |> should.equal("")

  "gleam"
  |> string.slice(at_index: 2, length: -3)
  |> should.equal("")
}

pub fn crop_test() {
  "gleam"
  |> string.crop("gl")
  |> should.equal("gleam")

  "gleam"
  |> string.crop("le")
  |> should.equal("leam")

  string.crop(from: "gleam", before: "ea")
  |> should.equal("eam")

  "gleam"
  |> string.crop("")
  |> should.equal("gleam")

  "gleam"
  |> string.crop("!")
  |> should.equal("gleam")
}

pub fn drop_left_test() {
  "gleam"
  |> string.drop_left(up_to: 2)
  |> should.equal("eam")

  "gleam"
  |> string.drop_left(up_to: 6)
  |> should.equal("")

  "gleam"
  |> string.drop_left(up_to: -2)
  |> should.equal("gleam")
}

pub fn drop_right_test() {
  "gleam"
  |> string.drop_right(up_to: 2)
  |> should.equal("gle")

  "gleam"
  |> string.drop_right(up_to: 5)
  |> should.equal("")

  "gleam"
  |> string.drop_right(up_to: -2)
  |> should.equal("gleam")
}

pub fn pad_left_test() {
  "121"
  |> string.pad_left(to: 5, with: ".")
  |> should.equal("..121")

  "121"
  |> string.pad_left(to: 3, with: ".")
  |> should.equal("121")

  "121"
  |> string.pad_left(to: 2, with: ".")
  |> should.equal("121")

  "121"
  |> string.pad_left(to: 4, with: "XY")
  |> should.equal("X121")

  "121"
  |> string.pad_left(to: 5, with: "XY")
  |> should.equal("XY121")

  "121"
  |> string.pad_left(to: 6, with: "XY")
  |> should.equal("XYX121")
}

pub fn pad_right_test() {
  "121"
  |> string.pad_right(to: 5, with: ".")
  |> should.equal("121..")

  "121"
  |> string.pad_right(to: 3, with: ".")
  |> should.equal("121")

  "121"
  |> string.pad_right(to: 2, with: ".")
  |> should.equal("121")

  "121"
  |> string.pad_right(to: 4, with: "XY")
  |> should.equal("121X")

  "121"
  |> string.pad_right(to: 5, with: "XY")
  |> should.equal("121XY")

  "121"
  |> string.pad_right(to: 6, with: "XY")
  |> should.equal("121XYX")
}

pub fn pop_grapheme_test() {
  "gleam"
  |> string.pop_grapheme()
  |> should.equal(Ok(#("g", "leam")))

  "g"
  |> string.pop_grapheme()
  |> should.equal(Ok(#("g", "")))

  ""
  |> string.pop_grapheme()
  |> should.equal(Error(Nil))
}

pub fn to_graphemes_test() {
  "abc"
  |> string.to_graphemes()
  |> should.equal(["a", "b", "c"])

  "a"
  |> string.to_graphemes()
  |> should.equal(["a"])

  ""
  |> string.to_graphemes()
  |> should.equal([])
}

pub fn utf_codepoint_test() {
  string.utf_codepoint(1114444)
  |> should.be_error

  string.utf_codepoint(65534)
  |> should.be_error

  string.utf_codepoint(55296)
  |> should.be_error
}

pub fn bit_string_utf_codepoint_test() {
  assert Ok(snake) = string.utf_codepoint(128013)
  should.equal(<<snake:utf8_codepoint>>, <<"🐍":utf8>>)
}

pub fn to_option_test() {
  ""
  |> string.to_option
  |> should.equal(None)

  "ok"
  |> string.to_option
  |> should.equal(Some("ok"))
}
