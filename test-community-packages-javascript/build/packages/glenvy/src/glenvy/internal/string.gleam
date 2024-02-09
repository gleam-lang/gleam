//// Extensions to `gleam/string`.

import gleam/string

/// Returns the given string after trimming the indicated characters from the left. 
pub fn trim_chars_left(value: String, trim chars_to_trim: String) -> String {
  case
    string.is_empty(chars_to_trim) || !string.starts_with(value, chars_to_trim)
  {
    True -> value
    False ->
      value
      |> string.slice(
        at_index: string.length(chars_to_trim),
        length: string.length(value),
      )
  }
}

/// Returns the given string after trimming the indicated characters from the right.
pub fn trim_chars_right(value: String, trim chars_to_trim: String) -> String {
  case
    string.is_empty(chars_to_trim) || !string.ends_with(value, chars_to_trim)
  {
    True -> value
    False ->
      value
      |> string.slice(
        at_index: 0,
        length: string.length(value) - string.length(chars_to_trim),
      )
  }
}
