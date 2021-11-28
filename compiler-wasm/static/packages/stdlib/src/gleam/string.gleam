//// Strings in Gleam are UTF-8 binaries. They can be written in your code as
//// text surrounded by `"double quotes"`.

import gleam/string_builder
import gleam/iterator.{Iterator}
import gleam/list
import gleam/order
import gleam/result
import gleam/option.{None, Option, Some}

if erlang {
  import gleam/dynamic.{Dynamic}
}

/// Determines if a string is empty.
///
/// ## Examples
///
///    > is_empty("")
///    True
///
///    > is_empty("the world")
///    False
///
pub fn is_empty(str: String) -> Bool {
  str == ""
}

/// Gets the number of grapheme clusters in a given string.
///
/// This function has to iterate across the whole string to count the number of
/// graphemes, so it runs in linear time.
///
/// ## Examples
///
///    > length("Gleam")
///    5
///
///    > length("ß↑e̊")
///    3
///
///    > length("")
///    0
///
pub fn length(string: String) -> Int {
  do_length(string)
}

if erlang {
  external fn do_length(String) -> Int =
    "string" "length"
}

if javascript {
  external fn do_length(String) -> Int =
    "../gleam_stdlib.js" "string_length"
}

///
/// Reverses a string.
///
/// This function has to iterate across the whole string so it runs in linear
/// time.
///
/// ## Examples
///
///    > reverse("stressed")
///    "desserts"
///
pub fn reverse(string: String) -> String {
  string
  |> string_builder.from_string
  |> string_builder.reverse
  |> string_builder.to_string
}

/// Creates a new string by replacing all occurrences of a given substring.
///
/// ## Examples
///
///    > replace("www.example.com", each: ".", with: "-")
///    "www-example-com"
///
///    > replace("a,b,c,d,e", each: ",", with: "/")
///    "a/b/c/d/e"
///
pub fn replace(
  in string: String,
  each pattern: String,
  with substitute: String,
) -> String {
  string
  |> string_builder.from_string
  |> string_builder.replace(each: pattern, with: substitute)
  |> string_builder.to_string
}

/// Creates a new string with all the graphemes in the input string converted to
/// lowercase.
///
/// Useful for case-insensitive comparisons.
///
/// ## Examples
///
///    > lowercase("X-FILES")
///    "x-files"
///
pub fn lowercase(string: String) -> String {
  do_lowercase(string)
}

if erlang {
  external fn do_lowercase(String) -> String =
    "string" "lowercase"
}

if javascript {
  external fn do_lowercase(String) -> String =
    "../gleam_stdlib.js" "lowercase"
}

/// Creates a new string with all the graphemes in the input string converted to
/// uppercase.
///
/// Useful for case-insensitive comparisons and VIRTUAL YELLING.
///
/// ## Examples
///
///    > uppercase("skinner")
///    "SKINNER"
///
pub fn uppercase(string: String) -> String {
  do_uppercase(string)
}

if erlang {
  external fn do_uppercase(String) -> String =
    "string" "uppercase"
}

if javascript {
  external fn do_uppercase(String) -> String =
    "../gleam_stdlib.js" "uppercase"
}

/// Compares two strings to see which is "larger" by comparing their graphemes.
///
/// This does not compare the size or length of the given strings.
///
/// ## Examples
///
///    > compare("Anthony", "Anthony")
///    order.Eq
///
///    > compare("A", "B")
///    order.Lt
///
pub fn compare(a: String, b: String) -> order.Order {
  case a == b {
    True -> order.Eq
    _ ->
      case less_than(a, b) {
        True -> order.Lt
        _ -> order.Gt
      }
  }
}

if erlang {
  external fn less_than(String, String) -> Bool =
    "gleam_stdlib" "less_than"
}

if javascript {
  external fn less_than(String, String) -> Bool =
    "../gleam_stdlib.js" "less_than"
}

/// Takes a substring given a start and end Grapheme indexes. Negative indexes
/// are taken starting from the *end* of the list.
///
/// ## Examples
///    > slice(from: "gleam", at_index: 1, length: 2)
///    "le"
///
///    > slice(from: "gleam", at_index: 1, length: 10)
///    "leam"
///
///    > slice(from: "gleam", at_index: 10, length: 3)
///    ""
///
///    > slice(from: "gleam", at_index: -2, length: 2)
///    "am"
///
///    > slice(from: "gleam", at_index: -12, length: 2)
///    ""
///
pub fn slice(from string: String, at_index idx: Int, length len: Int) -> String {
  case len < 0 {
    True -> ""
    False ->
      case idx < 0 {
        True -> {
          let translated_idx = length(string) + idx
          case translated_idx < 0 {
            True -> ""
            False -> do_slice(string, translated_idx, len)
          }
        }
        False -> do_slice(string, idx, len)
      }
  }
}

if erlang {
  external fn do_slice(String, Int, Int) -> String =
    "string" "slice"
}

if javascript {
  external fn do_slice(String, Int, Int) -> String =
    "../gleam_stdlib.js" "slice_string"
}

/// Drops contents of the first string that occur before the second string.
/// If the first string does not contain the second string, the first string is returned.
///
/// ## Examples
///    > crop(from: "The Lone Gunmen", before: "Lone")
///    "Lone Gunmen"
///
pub fn crop(from string: String, before substring: String) -> String {
  do_crop(string, substring)
}

if erlang {
  fn do_crop(string: String, substring: String) -> String {
    string
    |> erl_contains(substring)
    |> dynamic.string()
    |> result.unwrap(string)
  }

  external fn erl_contains(String, String) -> Dynamic =
    "string" "find"
}

if javascript {
  external fn do_crop(String, String) -> String =
    "../gleam_stdlib.js" "crop_string"
}

/// Drops *n* Graphemes from the left side of a string.
///
/// ## Examples
///    > drop_left(from: "The Lone Gunmen", up_to: 2)
///    "e Lone Gunmen"
///
pub fn drop_left(from string: String, up_to num_graphemes: Int) -> String {
  case num_graphemes < 0 {
    True -> string
    False -> slice(string, num_graphemes, length(string) - num_graphemes)
  }
}

/// Drops *n* Graphemes from the right side of a string.
///
/// ## Examples
///    > drop_right(from: "Cigarette Smoking Man", up_to: 2)
///    "Cigarette Smoking M"
///
pub fn drop_right(from string: String, up_to num_graphemes: Int) -> String {
  case num_graphemes < 0 {
    True -> string
    False -> slice(string, 0, length(string) - num_graphemes)
  }
}

/// Checks if the first string contains the second.
///
/// ## Examples
///
///    > contains(does: "theory", contain: "ory")
///    True
///
///    > contains(does: "theory", contain: "the")
///    True
///
///    > contains(does: "theory", contain: "THE")
///    False
///
pub fn contains(does haystack: String, contain needle: String) -> Bool {
  do_contains(haystack, needle)
}

if erlang {
  fn do_contains(haystack: String, needle: String) -> Bool {
    haystack
    |> erl_contains(needle)
    |> dynamic.bit_string
    |> result.is_ok
  }
}

if javascript {
  fn do_contains(haystack: String, needle: String) -> Bool {
    index_of(haystack, needle) != -1
  }

  external fn index_of(String, String) -> Int =
    "../gleam_stdlib.js" "index_of"
}

/// Checks whether the first string starts with the second one.
///
/// ## Examples
///
///    > starts_with("theory", "ory")
///    False
///
pub fn starts_with(string: String, prefix: String) -> Bool {
  do_starts_with(string, prefix)
}

if erlang {
  external fn do_starts_with(String, String) -> Bool =
    "gleam_stdlib" "string_starts_with"
}

if javascript {
  external fn do_starts_with(String, String) -> Bool =
    "../gleam_stdlib.js" "starts_with"
}

/// Checks whether the first string ends with the second one.
///
/// ## Examples
///
///    > ends_with("theory", "ory")
///    True
///
pub fn ends_with(string: String, suffix: String) -> Bool {
  do_ends_with(string, suffix)
}

if erlang {
  external fn do_ends_with(String, String) -> Bool =
    "gleam_stdlib" "string_ends_with"
}

if javascript {
  external fn do_ends_with(String, String) -> Bool =
    "../gleam_stdlib.js" "ends_with"
}

/// Creates a list of strings by splitting a given string on a given substring.
///
/// ## Examples
///
///    > split("home/gleam/desktop/", on: "/")
///    ["home", "gleam", "desktop", ""]
///
pub fn split(x: String, on substring: String) -> List(String) {
  x
  |> string_builder.from_string
  |> string_builder.split(on: substring)
  |> list.map(with: string_builder.to_string)
}

/// Splits a string a single time on the given substring.
///
/// Returns an error if substring not present.
///
/// ## Examples
///
///    > split_once("home/gleam/desktop/", on: "/")
///    Ok(#("home", "gleam/desktop/"))
///
///    > split_once("home/gleam/desktop/", on: "?")
///    Error(Nil)
///
pub fn split_once(
  x: String,
  on substring: String,
) -> Result(#(String, String), Nil) {
  do_split_once(x, substring)
}

if erlang {
  external fn erl_split(String, String) -> List(String) =
    "string" "split"

  fn do_split_once(
    x: String,
    substring: String,
  ) -> Result(#(String, String), Nil) {
    case erl_split(x, substring) {
      [first, rest] -> Ok(#(first, rest))
      _ -> Error(Nil)
    }
  }
}

if javascript {
  external fn do_split_once(
    x: String,
    substring: String,
  ) -> Result(#(String, String), Nil) =
    "../gleam_stdlib.js" "split_once"
}

/// Creates a new string by joining two strings together.
///
/// This function copies both strings and runs in linear time. If you find
/// yourself joining strings frequently consider using the [string_builder](../string_builder)
/// module as it can append strings much faster!
///
/// ## Examples
///
///    > append(to: "butter", suffix: "fly")
///    "butterfly"
///
pub fn append(to first: String, suffix second: String) -> String {
  first
  |> string_builder.from_string
  |> string_builder.append(second)
  |> string_builder.to_string
}

/// Creates a new string by joining many strings together.
///
/// This function copies both strings and runs in linear time. If you find
/// yourself joining strings frequently consider using the [string_builder](../string_builder)
/// module as it can append strings much faster!
///
/// ## Examples
///
///    > concat(["never", "the", "less"])
///    "nevertheless"
///
pub fn concat(strings: List(String)) -> String {
  strings
  |> string_builder.from_strings
  |> string_builder.to_string
}

/// Creates a new string by repeating a string a given number of times.
///
/// This function runs in linear time.
///
/// ## Examples
///
///    > repeat("ha", times: 3)
///    "hahaha"
///
pub fn repeat(string: String, times times: Int) -> String {
  iterator.repeat(string)
  |> iterator.take(times)
  |> iterator.to_list
  |> concat
}

/// Joins many strings together with a given separator.
///
/// This function runs in linear time.
///
/// ## Examples
///
///    > join(["home","evan","Desktop"], with: "/")
///    "home/evan/Desktop"
///
pub fn join(strings: List(String), with separator: String) -> String {
  strings
  |> list.intersperse(with: separator)
  |> concat
}

/// Pads a string on the left until it has at least given number of Graphemes.
///
/// ## Examples
///
///    > pad_left("121", to: 5, with: ".")
///    "..121"
///
///    > pad_left("121", to: 3, with: ".")
///    "121"
///
///    > pad_left("121", to: 2, with: ".")
///    "121"
///
pub fn pad_left(string: String, to desired_length: Int, with pad_string: String) {
  let current_length = length(string)
  let to_pad_length = desired_length - current_length
  padding(to_pad_length, pad_string)
  |> iterator.append(iterator.single(string))
  |> iterator.to_list
  |> concat
}

/// Pads a string on the right until it has a given length.
///
/// ## Examples
///
///    > pad_right("121", to: 5, with: ".")
///    "121.."
///
///    > pad_right("121", to: 3, with: ".")
///    "121"
///
///    > pad_right("121", to: 2, with: ".")
///    "121"
///
pub fn pad_right(
  string: String,
  to desired_length: Int,
  with pad_string: String,
) {
  let current_length = length(string)
  let to_pad_length = desired_length - current_length
  iterator.single(string)
  |> iterator.append(padding(to_pad_length, pad_string))
  |> iterator.to_list
  |> concat
}

fn padding(size: Int, pad_string: String) -> Iterator(String) {
  let pad_length = length(pad_string)
  let num_pads = size / pad_length
  let extra = size % pad_length
  iterator.repeat(pad_string)
  |> iterator.take(num_pads)
  |> iterator.append(iterator.single(slice(pad_string, 0, extra)))
}

/// Removes whitespace on both sides of a String.
///
/// ## Examples
///
///    > trim("  hats  \n")
///    "hats"
///
pub fn trim(string: String) -> String {
  do_trim(string)
}

if erlang {
  fn do_trim(string: String) -> String {
    erl_trim(string, Both)
  }

  type Direction {
    Leading
    Trailing
    Both
  }

  external fn erl_trim(String, Direction) -> String =
    "string" "trim"
}

if javascript {
  external fn do_trim(string: String) -> String =
    "../gleam_stdlib.js" "trim"
}

/// Removes whitespace on the left of a String.
///
/// ## Examples
///
///    > trim_left("  hats  \n")
///    "hats  \n"
///
pub fn trim_left(string: String) -> String {
  do_trim_left(string)
}

if erlang {
  fn do_trim_left(string: String) -> String {
    erl_trim(string, Leading)
  }
}

if javascript {
  external fn do_trim_left(string: String) -> String =
    "../gleam_stdlib.js" "trim_left"
}

/// Removes whitespace on the right of a String.
///
/// ## Examples
///
///    > trim_right("  hats  \n")
///    "  hats"
///
pub fn trim_right(string: String) -> String {
  do_trim_right(string)
}

if erlang {
  fn do_trim_right(string: String) -> String {
    erl_trim(string, Trailing)
  }
}

if javascript {
  external fn do_trim_right(string: String) -> String =
    "../gleam_stdlib.js" "trim_right"
}

/// Splits a non-empty string into its head and tail. This lets you
/// pattern match on strings exactly as you would with lists.
///
/// ## Examples
///    > pop_grapheme("gleam")
///    Ok(#("g", "leam"))
///
///    > pop_grapheme("")
///    Error(Nil)
///
pub fn pop_grapheme(string: String) -> Result(#(String, String), Nil) {
  do_pop_grapheme(string)
}

if erlang {
  external fn do_pop_grapheme(string: String) -> Result(#(String, String), Nil) =
    "gleam_stdlib" "string_pop_grapheme"
}

if javascript {
  external fn do_pop_grapheme(string: String) -> Result(#(String, String), Nil) =
    "../gleam_stdlib.js" "pop_grapheme"
}

/// Converts a string to a list of Graphemes.
///
///    > to_graphemes("abc")
///    ["a", "b", "c"]
///
pub fn to_graphemes(string: String) -> List(String) {
  case pop_grapheme(string) {
    Ok(#(grapheme, rest)) -> [grapheme, ..to_graphemes(rest)]
    _ -> []
  }
}

if erlang {
  external fn unsafe_int_to_utf_codepoint(Int) -> UtfCodepoint =
    "gleam_stdlib" "identity"
}

if javascript {
  external fn unsafe_int_to_utf_codepoint(Int) -> UtfCodepoint =
    "../gleam_stdlib.js" "codepoint"
}

/// Converts an integer to a UtfCodepoint
///
/// Returns an error if the integer does not represent a valid UTF codepoint.
///
pub fn utf_codepoint(value: Int) -> Result(UtfCodepoint, Nil) {
  case value {
    i if i > 1114111 -> Error(Nil)
    65534 | 65535 -> Error(Nil)
    i if i >= 55296 && i <= 57343 -> Error(Nil)
    i -> Ok(unsafe_int_to_utf_codepoint(i))
  }
}

/// Convert a string into an optional string where an empty string becomes `None`.
///
/// ## Examples
///
/// ```gleam
/// > to_option("")
/// None
/// ```
///
/// ```gleam
/// > to_option("")
/// None
/// ```
///
pub fn to_option(s: String) -> Option(String) {
  case s {
    "" -> None
    _ -> Some(s)
  }
}
