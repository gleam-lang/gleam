//// Strongly-typed access to environment variables.

import gleam/float
import gleam/int
import gleam/result.{try}
import gleam/string
import glenvy/internal/os

/// Returns the value for the environment variable with the given name as a `String`.
pub fn get_string(name: String) -> Result(String, Nil) {
  os.get_env(name)
}

/// Returns the value for the environment variable with the given name.
///
/// Uses the provided `parser` to parse the value.
///
/// Returns `Error(Nil)` if the provided `parser` returns `Error(Nil)`.
pub fn get(
  name: String,
  parser parse: fn(String) -> Result(a, Nil),
) -> Result(a, Nil) {
  use value <- try(get_string(name))

  value
  |> parse
}

/// Returns the value for the environment variable with the given name as an `Int`.
///
/// Returns `Error(Nil)` if the environment variable cannot be parsed as an `Int`.
pub fn get_int(name: String) -> Result(Int, Nil) {
  name
  |> get(parser: int.parse)
}

/// Returns the value for the environment variable with the given name as a `Float`.
///
/// Returns `Error(Nil)` if the environment variable cannot be parsed as a `Float`.
pub fn get_float(name: String) -> Result(Float, Nil) {
  name
  |> get(parser: float.parse)
}

/// Returns the value for the environment variable with the given name as a `Bool`.
///
/// The following values are parsed as `True`:
/// - `true`
/// - `t`
/// - `yes`
/// - `y`
/// - `1`
///
/// The following values are pased as `False`:
/// - `false`
/// - `f`
/// - `no`
/// - `n`
/// - `0`
///
/// The parsing is case-insensitive.
///
/// Returns `Error(Nil)` if the environment variable cannot be parsed as a `Bool`.
///
/// Use `get` if you want to provide your own parser.
pub fn get_bool(name: String) -> Result(Bool, Nil) {
  let parse_bool = fn(value) {
    let value =
      value
      |> string.lowercase

    case value {
      "true" | "t" | "yes" | "y" | "1" -> Ok(True)
      "false" | "f" | "no" | "n" | "0" -> Ok(False)
      _ -> Error(Nil)
    }
  }

  name
  |> get(parser: parse_bool)
}
