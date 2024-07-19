use crate::assert_erl;

#[test]
fn unicode1() {
    assert_erl!(
        r#"
pub fn emoji() -> String {
  "\u{1f600}"
}
"#,
    );
}

#[test]
fn unicode2() {
    assert_erl!(
        r#"
pub fn y_with_dieresis() -> String {
  "\u{0308}y"
}
"#,
    );
}

#[test]
fn unicode_concat1() {
    assert_erl!(
        r#"
pub fn main(x) -> String {
  x <> "\u{0308}"
}
"#,
    );
}

#[test]
fn unicode_concat2() {
    assert_erl!(
        r#"
pub fn main(x) -> String {
  x <> "\\u{0308}"
}
"#,
    );
}

#[test]
fn unicode_concat3() {
    assert_erl!(
        r#"
pub fn main(x) -> String {
  x <> "\\\u{0308}"
}
"#,
    );
}

#[test]
fn not_unicode_escape_sequence() {
    // '\u'-s must be converted to '\x' in the Erlang codegen.
    // but '\\u'-s mustn't.
    assert_erl!(
        r#"
pub fn not_unicode_escape_sequence() -> String {
  "\\u{03a9}"
}
"#,
    );
}

#[test]
fn not_unicode_escape_sequence2() {
    assert_erl!(
        r#"
pub fn not_unicode_escape_sequence() -> String {
  "\\\\u{03a9}"
}
"#,
    );
}

#[test]
fn unicode3() {
    assert_erl!(
        r#"
pub fn y_with_dieresis_with_slash() -> String {
  "\\\u{0308}y"
}
"#,
    );
}

#[test]
fn unicode_escape_sequence_6_digits() {
    assert_erl!(
        r#"
pub fn unicode_escape_sequence_6_digits() -> String {
  "\u{10abcd}"
}
"#,
    );
}

#[test]
fn ascii_as_unicode_escape_sequence() {
    assert_erl!(
        r#"
pub fn y() -> String {
  "\u{79}"
}
"#,
    )
}

#[test]
fn concat() {
    assert_erl!(
        r#"
pub fn go(x, y) {
  x <> y
}
"#,
    );
}

#[test]
fn concat_3_variables() {
    assert_erl!(
        r#"
pub fn go(x, y, z) {
  x <> y <> z
}
"#,
    );
}

#[test]
fn string_prefix() {
    assert_erl!(
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
fn string_prefix_assignment() {
    assert_erl!(
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
fn string_prefix_assignment_with_guard() {
    assert_erl!(
        r#"
pub fn go(x) {
  case x {
    "Hello, " as greeting <> name if name == "Dude" -> greeting <> "Mate"
    "Hello, " as greeting <> name -> greeting
    _ -> "Unknown"
  }
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/3126
#[test]
fn string_prefix_assignment_with_escape_sequences() {
    assert_erl!(
        r#"
pub fn go(x) {
  let _ = case x {
    "\f" as start <> rest -> "test"
    "\n" as start <> rest -> "test"
    "\r" as start <> rest -> "test"
    "\t" as start <> rest -> "test"
    "\"" as start <> rest -> "test"
    "\\" as start <> rest -> "test"
    "\f \n \r \t \" \\" as start <> rest -> "control chars with prefix assignment"
    "\u{9}" as start <> rest -> "test"
    "\u{000009}" as start <> rest -> "test"
    "\u{21}" as start <> rest -> "test"
    "\u{100}" as start <> rest -> "test"
    "\u{1000}" as start <> rest -> "test"
    "\u{1F600}" as start <> rest -> "test"
    "\u{1f600}" as start <> rest -> "test"
    "\u{01F600}" as start <> rest -> "test"
    "\u{01f600}" as start <> rest -> "test"
    "\u{9} \u{000009} \u{21} \u{100} \u{1000} \u{1F600} \u{01F600}" as start <> rest -> "test"
    _ -> "Unknown"
  }
}
"#,
    )
}

#[test]
fn string_prefix_with_escape_sequences() {
    assert_erl!(
        r#"
pub fn go(x) {
  let _ = case x {
    "\f" <> rest -> "test"
    "\n" <> rest -> "test"
    "\r" <> rest -> "test"
    "\t" <> rest -> "test"
    "\"" <> rest -> "test"
    "\\" <> rest -> "test"
    "\f \n \r \t \" \\" <> rest -> "control chars with prefix assignment"
    "\u{9}" <> rest -> "test"
    "\u{000009}" <> rest -> "test"
    "\u{21}" <> rest -> "test"
    "\u{100}" <> rest -> "test"
    "\u{1000}" <> rest -> "test"
    "\u{1F600}" <> rest -> "test"
    "\u{1f600}" <> rest -> "test"
    "\u{01F600}" <> rest -> "test"
    "\u{01f600}" <> rest -> "test"
    "\u{9} \u{000009} \u{21} \u{100} \u{1000} \u{1F600} \u{01F600}" <> rest -> "test"
    _ -> "Unknown"
  }
}
"#,
    )
}

#[test]
fn string_prefix_assignment_not_unicode_escape_sequence() {
    assert_erl!(
        r#"
pub fn go(x) {
  let _ = case x {
    "\\u{9}" as start <> rest -> "test"
    "\\u{000009}" as start <> rest -> "test"
    "\\u{21}" as start <> rest -> "test"
    "\\u{100}" as start <> rest -> "test"
    "\\u{1000}" as start <> rest -> "test"
    "\\u{1F600}" as start <> rest -> "test"
    "\\u{1f600}" as start <> rest -> "test"
    "\\u{01F600}" as start <> rest -> "test"
    "\\u{01f600}" as start <> rest -> "test"
    "\\u{9} \\u{000009} \\u{21} \\u{100} \\u{1000} \\u{1F600} \\u{01F600}" as start <> rest -> "test"
    _ -> "Unknown"
  }
}
"#,
    )
}

#[test]
fn string_prefix_not_unicode_escape_sequence() {
    assert_erl!(
        r#"
pub fn go(x) {
  let _ = case x {
    "\\u{9}" <> rest -> "test"
    "\\u{000009}" <> rest -> "test"
    "\\u{21}" <> rest -> "test"
    "\\u{100}" <> rest -> "test"
    "\\u{1000}" <> rest -> "test"
    "\\u{1F600}" <> rest -> "test"
    "\\u{1f600}" <> rest -> "test"
    "\\u{01F600}" <> rest -> "test"
    "\\u{01f600}" <> rest -> "test"
    "\\u{9} \\u{000009} \\u{21} \\u{100} \\u{1000} \\u{1F600} \\u{01F600}" <> rest -> "test"
    _ -> "Unknown"
  }
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/2471
#[test]
fn string_prefix_assignment_with_multiple_subjects() {
    assert_erl!(
        r#"
pub fn go(x) {
  case x {
    "1" as digit <> _ | "2" as digit <> _ -> digit
    _ -> "Unknown"
  }
}
"#,
    )
}

#[test]
fn string_prefix_shadowing() {
    assert_erl!(
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

#[test]
fn rest_variable_rewriting() {
    // This test checks that the variable on the right hand side of <> has
    // it's name written correctly when it shadows an existing variable
    assert_erl!(
        r#"
pub fn go(x) {
  case x {
    "Hello, " <> x -> x
    _ -> "Unknown"
  }
}
"#,
    );
}

#[test]
fn discard_concat_rest_pattern() {
    // We can discard the right hand side, it parses and type checks ok
    assert_erl!(
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
fn string_of_number_concat() {
    assert_erl!(
        r#"
pub fn go(x) {
  x <> "1"
}
"#,
    );
}

#[test]
fn concat_function_call() {
    assert_erl!(
        r#"
fn x() {
  ""
}

pub fn go() {
  x() <> x()
}
"#,
    );
}

#[test]
fn concat_constant() {
    assert_erl!(
        r#"
const a = "Hello, "
const b = "Joe!"

pub fn go() {
  a <> b
}
"#,
    );
}

#[test]
fn concat_constant_fn() {
    assert_erl!(
        r#"
const cs = s

fn s() {
  "s"
}

pub fn go() {
  cs() <> cs()
}
"#,
    );
}

#[test]
fn pipe_concat() {
    assert_erl!(
        r#"
fn id(x) {
  x
}

pub fn main() {
  { "" |> id } <> { "" |> id }
}
"#,
    );
}

#[test]
fn assert_string_prefix() {
    assert_erl!(
        r#"
pub fn main(x) {
  let assert "m-" <> rest = x
  rest
}
"#,
    );
}

#[test]
fn assert_string_prefix_discar() {
    assert_erl!(
        r#"
pub fn main(x) {
  let assert "m-" <> _ = x
}
"#,
    );
}

#[test]
fn assert_const_concat() {
    assert_erl!(
        r#"
const cute = "cute"
const cute_bee = cute <> "bee"

pub fn main() {
  cute_bee
}
"#
    );
}

#[test]
fn assert_const_concat_many_strings() {
    assert_erl!(
        r#"
const big_concat = "a" <> "b" <> "c" <> "d" <> "e" <> "f" <> "g" <> "h" <> "i" <> "j" <> "k" <> "l" <> "m" <> "n" <> "o" <> "p" <> "q" <> "r" <> "s" <> "t" <> "u" <> "v" <> "w" <> "x" <> "y" <> "z"

pub fn main() {
  big_concat
}
"#
    );
}

#[test]
fn assert_const_concat_many_strings_in_list() {
    assert_erl!(
        r#"
const big_concat_list = ["a" <> "b" <> "c" <> "d" <> "e" <> "f" <> "g" <> "h" <> "i" <> "j" <> "k" <> "l" <> "m" <> "n" <> "o" <> "p" <> "q" <> "r" <> "s" <> "t" <> "u" <> "v" <> "w" <> "x" <> "y" <> "z"]

pub fn main() {
  big_concat_list
}
"#
    );
}

#[test]
fn assert_const_concat_other_const_concat() {
    assert_erl!(
        r#"
const cute_bee = "cute" <> "bee"
const cute_cute_bee_buzz = cute_bee <> "buzz"

pub fn main() {
  cute_cute_bee_buzz
}
"#
    );
}
