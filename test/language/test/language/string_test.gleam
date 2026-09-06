// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

fn id(string: String) -> String {
  string
}

pub fn empty_strings_equal_test() {
  assert id("") == id("")
}

pub fn newlines_equal_test() {
  assert id(
      "
",
    )
    == id("\n")
}

pub fn let_assert_string_prefix_test() {
  let assert "ab" <> rest = "abcdef"
  assert "cdef" == rest
}

pub fn concat_block_single_test() {
  let name = id("Lucy")
  let result = name <> { "/" } <> name
  assert result == "Lucy/Lucy"
}

pub fn concat_block_multiple_test() {
  let name = id("Lucy")
  let result =
    name
    <> {
      let x = "/"
      x
    }
    <> name
  assert result == "Lucy/Lucy"
}
