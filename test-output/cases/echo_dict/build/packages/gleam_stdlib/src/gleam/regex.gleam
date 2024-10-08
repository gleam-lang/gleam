//// This module contains regular expression matching functions for strings.
//// The matching algorithms of the library are based on the PCRE library, but not
//// all of the PCRE library is interfaced and some parts of the library go beyond
//// what PCRE offers. Currently PCRE version 8.40 (release date 2017-01-11) is used.

import gleam/option.{type Option}

pub type Regex

/// The details about a particular match:
///
pub type Match {
  Match(
    /// The full string of the match.
    content: String,
    /// A `Regex` can have subpatterns, sup-parts that are in parentheses.
    submatches: List(Option(String)),
  )
}

/// When a regular expression fails to compile:
///
pub type CompileError {
  CompileError(
    /// The problem encountered that caused the compilation to fail
    error: String,
    /// The byte index into the string to where the problem was found
    /// This value may not be correct in JavaScript environments.
    byte_index: Int,
  )
}

pub type Options {
  Options(case_insensitive: Bool, multi_line: Bool)
}

/// Creates a `Regex` with some additional options.
///
/// ## Examples
///
/// ```gleam
/// let options = Options(case_insensitive: False, multi_line: True)
/// let assert Ok(re) = compile("^[0-9]", with: options)
/// check(re, "abc\n123")
/// // -> True
/// ```
///
/// ```gleam
/// let options = Options(case_insensitive: True, multi_line: False)
/// let assert Ok(re) = compile("[A-Z]", with: options)
/// check(re, "abc123")
/// // -> True
/// ```
///
pub fn compile(
  pattern: String,
  with options: Options,
) -> Result(Regex, CompileError) {
  do_compile(pattern, options)
}

@external(erlang, "gleam_stdlib", "compile_regex")
@external(javascript, "../gleam_stdlib.mjs", "compile_regex")
fn do_compile(
  pattern: String,
  with with: Options,
) -> Result(Regex, CompileError)

/// Creates a new `Regex`.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(re) = from_string("[0-9]")
/// check(re, "abc123")
/// // -> True
/// ```
///
/// ```gleam
/// check(re, "abcxyz")
/// // -> False
/// ```
///
/// ```gleam
/// from_string("[0-9")
/// // -> Error(CompileError(
/// //   error: "missing terminating ] for character class",
/// //   byte_index: 4
/// // ))
/// ```
///
pub fn from_string(pattern: String) -> Result(Regex, CompileError) {
  compile(pattern, Options(case_insensitive: False, multi_line: False))
}

/// Returns a boolean indicating whether there was a match or not.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(re) = from_string("^f.o.?")
/// check(with: re, content: "foo")
/// // -> True
/// ```
///
/// ```gleam
/// check(with: re, content: "boo")
/// // -> False
/// ```
///
pub fn check(with regex: Regex, content string: String) -> Bool {
  do_check(regex, string)
}

@external(erlang, "gleam_stdlib", "regex_check")
@external(javascript, "../gleam_stdlib.mjs", "regex_check")
fn do_check(regex: Regex, string: String) -> Bool

/// Splits a string.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(re) = from_string(" *, *")
/// split(with: re, content: "foo,32, 4, 9  ,0")
/// // -> ["foo", "32", "4", "9", "0"]
/// ```
///
pub fn split(with regex: Regex, content string: String) -> List(String) {
  do_split(regex, string)
}

@external(erlang, "gleam_stdlib", "regex_split")
@external(javascript, "../gleam_stdlib.mjs", "regex_split")
fn do_split(regex: Regex, string: String) -> List(String)

/// Collects all matches of the regular expression.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(re) = from_string("[oi]n a (\\w+)")
/// scan(with: re, content: "I am on a boat in a lake.")
/// // -> [
/// //   Match(content: "on a boat", submatches: [Some("boat")]),
/// //   Match(content: "in a lake", submatches: [Some("lake")]),
/// // ]
/// ```
///
/// ```gleam
/// let assert Ok(re) = regex.from_string("([+|\\-])?(\\d+)(\\w+)?")
/// scan(with: re, content: "-36")
/// // -> [
/// //   Match(content: "-36", submatches: [Some("-"), Some("36")])
/// // ]
///
/// scan(with: re, content: "36")
/// // -> [
/// //   Match(content: "36", submatches: [None, Some("36")])
/// // ]
/// ```
///
/// ```gleam
/// let assert Ok(re) =
///   regex.from_string("var\\s*(\\w+)\\s*(int|string)?\\s*=\\s*(.*)")
/// scan(with: re, content: "var age = 32")
/// // -> [
/// //   Match(
/// //     content: "var age = 32",
/// //     submatches: [Some("age"), None, Some("32")],
/// //   ),
/// // ]
/// ```
///
/// ```gleam
/// let assert Ok(re) = regex.from_string("let (\\w+) = (\\w+)")
/// scan(with: re, content: "let age = 32")
/// // -> [
/// //   Match(
/// //     content: "let age = 32",
/// //     submatches: [Some("age"), Some("32")],
/// //   ),
/// // ]
///
/// scan(with: re, content: "const age = 32")
/// // -> []
/// ```
///
pub fn scan(with regex: Regex, content string: String) -> List(Match) {
  do_scan(regex, string)
}

@external(erlang, "gleam_stdlib", "regex_scan")
@external(javascript, "../gleam_stdlib.mjs", "regex_scan")
fn do_scan(regex: Regex, string: String) -> List(Match)

/// Creates a new `String` by replacing all substrings that match the regular
/// expression.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(re) = regex.from_string("^https://")
/// replace(each: re, in: "https://example.com", with: "www.")
/// // -> "www.example.com"
/// ```
///
/// ```gleam
/// let assert Ok(re) = regex.from_string("[, +-]")
/// replace(each: re, in: "a,b-c d+e", with: "/")
/// // -> "a/b/c/d/e"
/// ```
@external(erlang, "gleam_stdlib", "regex_replace")
@external(javascript, "../gleam_stdlib.mjs", "regex_replace")
pub fn replace(
  each pattern: Regex,
  in string: String,
  with substitute: String,
) -> String
