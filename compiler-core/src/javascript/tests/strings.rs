// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

use crate::assert_js;

#[test]
fn unicode1() {
    assert_js!(
        r#"
pub fn emoji() -> String {
  "\u{1f600}"
}
"#,
    );
}

#[test]
fn unicode2() {
    assert_js!(
        r#"
pub fn y_with_dieresis() -> String {
  "\u{0308}y"
}
"#,
    );
}

#[test]
fn ascii_as_unicode_escape_sequence() {
    assert_js!(
        r#"
pub fn y() -> String {
  "\u{79}"
}
"#,
    )
}

#[test]
fn unicode_escape_sequence_6_digits() {
    assert_js!(
        r#"
pub fn unicode_escape_sequence_6_digits() -> String {
  "\u{10abcd}"
}
"#,
    );
}

#[test]
fn string_literals() {
    assert_js!(
        r#"
pub fn go() {
  "Hello, Gleam!"
}
"#,
    );
}

#[test]
fn string_patterns() {
    assert_js!(
        r#"
pub fn go(x) {
  let assert "Hello" = x
}
"#,
    );
}

#[test]
fn equality() {
    assert_js!(
        r#"
pub fn go(a) {
  a == "ok"
  a != "ok"
  a == a
}
"#,
    );
}

#[test]
fn case() {
    assert_js!(
        r#"
pub fn go(a) {
  case a {
    "" -> 0
    "one" -> 1
    "two" -> 2
    _ -> 3
  }
}
"#,
    );
}

#[test]
fn string_concat() {
    assert_js!(
        r#"
pub fn go() {
  "Hello, " <> "Joe"
}
"#,
    );
}

#[test]
fn string_prefix() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    "Hello, " <> name -> name
    _ -> "Unknown"
  }
}
"#,
    );
}

#[test]
fn string_prefix_utf16() {
    assert_js!(
        r#"
pub fn go(x) {
  case "Θ wibble wobble" {
    "Θ" <> rest -> rest
    _ -> ""
  }
  case "🫥 is neutral dotted" {
    "🫥" <> rest -> rest
    _ -> ""
  }
  case "🇺🇸 is a cluster" {
    "🇺🇸" <> rest -> rest
    _ -> ""
  }
  case "\" is a an escaped quote" {
    "\"" <> rest -> rest
    _ -> ""
  }
  case "\\ is a an escaped backslash" {
    "\\" <> rest -> rest
    _ -> ""
  }
}
"#,
    );
}

#[test]
fn discard_concat_rest_pattern() {
    // We can discard the right hand side, it parses and type checks ok
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    "Hello, " <> _ -> Nil
    _ -> Nil
  }
}
"#,
    );
}

#[test]
fn string_prefix_assignment() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    "Hello, " as greeting <> name -> greeting
    _ -> "Unknown"
  }
}
"#,
    )
}

#[test]
fn string_prefix_assignment_with_utf_escape_sequence() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    "\u{0032} " as greeting <> name -> greeting
    "\u{0007ff} " as greeting <> name -> greeting
    "\u{00ffff} " as greeting <> name -> greeting
    "\u{10ffff} " as greeting <> name -> greeting
    _ -> "Unknown"
  }
}
"#,
    )
}

#[test]
fn string_prefix_shadowing() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    "Hello, " as x <> name -> x
    _ -> "Unknown"
  }
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/2471
#[test]
fn string_prefix_assignment_with_multiple_subjects() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    "1" as prefix <> _ | "11" as prefix <> _ -> prefix
    _ -> "Unknown"
  }
}
"#,
    )
}

#[test]
fn const_concat() {
    assert_js!(
        r#"
pub const cute = "cute"
pub const cute_bee = cute <> "bee"

pub fn main() {
  cute_bee
}
"#
    );
}

#[test]
fn const_concat_multiple() {
    assert_js!(
        r#"
pub const cute = "cute"
pub const cute_bee = cute <> "bee"
pub const cute_cute_bee_buzz = cute <> cute_bee <> "buzz"

pub fn main() {
  cute_cute_bee_buzz
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/5856
#[test]
fn overlapping_string_prefixes_with_guard() {
    assert_js!(
        r#"
pub fn classify(input: String, flag: Bool) -> String {
  case input {
    "aa" <> _rest if flag -> "wibble"
    "a" <> _rest if flag -> "wobble"
    "a" <> _rest -> "woo"
    _ -> "other"
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/5856
#[test]
fn string_prefix_binding_rest_with_guard() {
    assert_js!(
        r#"
pub fn classify(input: String, flag: Bool) -> String {
  case input {
    "aa" <> rest if flag -> rest
    "a" <> rest -> rest
    _ -> "other"
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/5856
#[test]
fn non_overlapping_sibling_string_prefix() {
    assert_js!(
        r#"
pub fn classify(input: String, a: Bool, b: Bool) -> String {
  case input {
    "aa" <> _ if a -> "aa"
    "a" <> _ if b -> "a-guard"
    "ac" <> _ -> "ac"
    _ -> "other"
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/5856
#[test]
fn exact_string_after_longer_prefix() {
    assert_js!(
        r#"
pub fn classify(input: String, a: Bool, b: Bool) -> String {
  case input {
    "aa" <> _ if a -> "aa-prefix"
    "a" <> _ if b -> "a-prefix"
    "a" -> "exact-a"
    _ -> "other"
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/5856
#[test]
fn string_prefix_name_binding_after_longer_prefix() {
    assert_js!(
        r#"
pub fn classify(input: String, flag: Bool) -> String {
  case input {
    "aa" <> _ if flag -> "aa"
    "a" as first <> rest -> rest <> first
    _ -> "other"
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/5856
#[test]
fn string_prefix_guard_on_bound_variable_falls_through() {
    assert_js!(
        r#"
pub fn classify(input: String) -> String {
  case input {
    "aa" <> rest if rest == "x" -> "aa-x"
    "a" <> rest -> rest
    _ -> "other"
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/5856
#[test]
fn string_prefix_nested_guard_falls_through_to_shorter() {
    assert_js!(
        r#"
pub fn classify(input: String, flag: Bool) -> String {
  case input {
    "w" <> _ if flag -> "w"
    "wibble" <> rest if rest == "x" -> rest
    "wib" <> rest -> rest
    _ -> "other"
  }
}
"#,
    );
}
