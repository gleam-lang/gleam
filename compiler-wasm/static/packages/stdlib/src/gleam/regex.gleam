//// This module contains regular expression matching functions for strings.
//// The matching algorithms of the library are based on the PCRE library, but not
//// all of the PCRE library is interfaced and some parts of the library go beyond
//// what PCRE offers. Currently PCRE version 8.40 (release date 2017-01-11) is used.

import gleam/option.{Option}

pub external type Regex

/// The details about a particular match:
///
pub type Match {
  Match(
    /// The full string of the match.
    content: String,
    /// A Regex can have subpatterns, sup-parts that are in parentheses.
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

/// Creates a Regex with some additional options.
///
/// ## Examples
///
///    > let options = Options(case_insensitive: False, multi_line: True)
///    > assert Ok(re) = compile("^[0-9]", with: options)
///    > match(re, "abc\n123")
///    True
///
///    > let options = Options(case_insensitive: True, multi_line: False)
///    > assert Ok(re) = compile("[A-Z]", with: options)
///    > match(re, "abc123")
///    True
///
pub fn compile(
  pattern: String,
  with options: Options,
) -> Result(Regex, CompileError) {
  do_compile(pattern, options)
}

if erlang {
  external fn do_compile(String, with: Options) -> Result(Regex, CompileError) =
    "gleam_stdlib" "compile_regex"
}

if javascript {
  external fn do_compile(String, with: Options) -> Result(Regex, CompileError) =
    "../gleam_stdlib.js" "compile_regex"
}

/// Creates a new Regex.
///
/// ## Examples
///
///    > assert Ok(re) = from_string("[0-9]")
///    > match(re, "abc123")
///    True
///
///    > match(re, "abcxyz")
///    False
///
///    > from_string("[0-9")
///    Error(
///      CompileError(
///        error: "missing terminating ] for character class",
///        byte_index: 4
///      )
///    )
///
pub fn from_string(pattern: String) -> Result(Regex, CompileError) {
  compile(pattern, Options(case_insensitive: False, multi_line: False))
}

/// Returns a boolean indicating whether there was a match or not.
///
/// ## Examples
///
///    > assert Ok(re) = from_string("^f.o.?")
///    > check(with: re, content: "foo")
///    True
///
///    > check(with: re, content: "boo")
///    False
///
pub fn check(with regex: Regex, content content: String) -> Bool {
  do_check(regex, content)
}

if erlang {
  external fn do_check(Regex, String) -> Bool =
    "gleam_stdlib" "regex_check"
}

if javascript {
  external fn do_check(Regex, String) -> Bool =
    "../gleam_stdlib.js" "regex_check"
}

/// Splits a string
///
/// ## Examples
///
///    > assert Ok(re) = from_string(" *, *")
///    > split(with: re, content: "foo,32, 4, 9  ,0")
///    ["foo", "32", "4", "9", "0"]
///
pub fn split(with regex: Regex, content string: String) -> List(String) {
  do_split(regex, string)
}

if erlang {
  external fn do_split(Regex, String) -> List(String) =
    "gleam_stdlib" "regex_split"
}

if javascript {
  fn do_split(regex, string) -> List(String) {
    js_split(string, regex)
  }

  external fn js_split(String, Regex) -> List(String) =
    "../gleam_stdlib.js" "split"
}

/// Collects all matches of the regular expression.
///
/// ## Examples
///
///    > assert Ok(re) = regex.from_string("[oi]n a (\\w+)")
///    > regex.scan(with: re, content: "I am on a boat in a lake.")
///    [
///      Match(
///        content: "on a boat",
///        submatches: [Some("boat")]
///      ),
///      Match(
///        content: "in a lake",
///        submatches: [Some("lake")]
///      )
///    ]
///
pub fn scan(with regex: Regex, content string: String) -> List(Match) {
  do_scan(regex, string)
}

if erlang {
  external fn do_scan(Regex, String) -> List(Match) =
    "gleam_stdlib" "regex_scan"
}

if javascript {
  external fn do_scan(Regex, String) -> List(Match) =
    "../gleam_stdlib.js" "regex_scan"
}
