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
    // This test checks that the the variable on the right hand side of <> has
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
