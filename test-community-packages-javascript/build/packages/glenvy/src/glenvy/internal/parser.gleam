//// A `.env` file parser.

import gleam/list
import gleam/map.{Map}
import gleam/result.{try}
import gleam/string
import glenvy/internal/string as glenvy_string
import glx/stringx

/// Parses a `.env` file into its contained environment variables.
pub fn parse_env_file(contents: String) -> Map(String, String) {
  let lines = stringx.lines(contents)

  let env_vars =
    lines
    |> list.filter_map(parse_line)
    |> map.from_list

  env_vars
}

/// Parses a single line from a `.env` file into the key and the value.
fn parse_line(line: String) -> Result(#(String, String), Nil) {
  use line <- try(skip_line_comment(line))

  use #(key, value) <- try(
    line
    |> string.split_once(on: "="),
  )

  use value <- try(strip_comments(value))

  let value =
    value
    |> string.trim
    |> unquote(quote: "'")
    |> unquote(quote: "\"")

  Ok(#(key, value))
}

/// Skips a line that starts with a comment.
fn skip_line_comment(line: String) -> Result(String, Nil) {
  case
    line
    |> string.starts_with("#")
  {
    False -> Ok(line)
    True -> Error(Nil)
  }
}

/// Strips the comments out of the given line.
fn strip_comments(line: String) -> Result(String, Nil) {
  case
    line
    |> string.split_once(on: "#")
  {
    Ok(#(value, _)) -> Ok(value)
    Error(Nil) -> Ok(line)
  }
}

/// Unquotes the given string using the specified quote character.
fn unquote(line: String, quote quote_char: String) -> String {
  line
  |> glenvy_string.trim_chars_left(quote_char)
  |> glenvy_string.trim_chars_right(quote_char)
}
