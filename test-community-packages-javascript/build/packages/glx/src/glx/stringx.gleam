//// Extensions to `gleam/string`.

import gleam/bool.{negate}
import gleam/list
import gleam/regex
import gleam/string

/// Returns the lines contained in the given string.
///
/// Supports both `\n` and `\r\n` line endings.
///
/// ## Examples
///
/// ```gleam
/// > lines("one\ntwo\n\nthree\n")
/// ["one", "two", "three"]
/// ```
///
/// ```gleam
/// > lines("one\r\ntwo\r\n\r\nthree\r\n")
/// ["one", "two", "three"]
/// ```
pub fn lines(value: String) -> List(String) {
  let assert Ok(newline_regex) = regex.from_string("\r?\n")

  let is_not_empty = fn(line) { negate(string.is_empty(line)) }

  value
  |> regex.split(with: newline_regex)
  |> list.filter(is_not_empty)
}
