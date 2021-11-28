import gleam/regex.{Match, Options}
import gleam/should
import gleam/io
import gleam/option.{None, Some}

pub fn from_string_test() {
  assert Ok(re) = regex.from_string("[0-9]")

  regex.check(re, "abc123")
  |> should.equal(True)

  regex.check(re, "abcxyz")
  |> should.equal(False)

  assert Error(_) = regex.from_string("[0-9")
}

pub fn compile_test() {
  let options = Options(case_insensitive: True, multi_line: False)
  assert Ok(re) = regex.compile("[A-B]", options)

  regex.check(re, "abc123")
  |> should.equal(True)

  let options = Options(case_insensitive: False, multi_line: True)
  assert Ok(re) = regex.compile("^[0-9]", options)

  regex.check(re, "abc\n123")
  |> should.equal(True)
}

pub fn check_test() {
  assert Ok(re) = regex.from_string("^f.o.?")

  regex.check(re, "foo")
  |> should.equal(True)

  regex.check(re, "boo")
  |> should.equal(False)
}

pub fn split_test() {
  assert Ok(re) = regex.from_string(" *, *")

  regex.split(re, "foo,32, 4, 9  ,0")
  |> should.equal(["foo", "32", "4", "9", "0"])
}

pub fn scan_test() {
  assert Ok(re) = regex.from_string("Gl\\w+")

  regex.scan(re, "!Gleam")
  |> should.equal([Match(content: "Gleam", submatches: [])])

  regex.scan(re, "हGleam")
  |> should.equal([Match(content: "Gleam", submatches: [])])

  regex.scan(re, "𐍈Gleam")
  |> should.equal([Match(content: "Gleam", submatches: [])])

  assert Ok(re) = regex.from_string("[oi]n a(.?) (\\w+)")

  regex.scan(re, "I am on a boat in a lake.")
  |> should.equal([
    Match(content: "on a boat", submatches: [None, Some("boat")]),
    Match(content: "in a lake", submatches: [None, Some("lake")]),
  ])
}
