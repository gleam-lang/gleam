use crate::assert_erl;

#[test]
fn one_var() {
    // One var
    assert_erl!(
        r#"pub fn go() {
  let assert Ok(y) = Ok(1)
  y
}"#
    );
}

#[test]
fn more_than_one_var() {
    // More vars
    assert_erl!(
        r#"pub fn go(x) {
  let assert [1, a, b, c] = x
  [a, b, c]
}"#
    );
}

#[test]
fn pattern_let() {
    // Pattern::Let
    assert_erl!(
        r#"pub fn go(x) {
  let assert [1 as a, b, c] = x
  [a, b, c]
}"#
    );
}

#[test]
fn variable_rewrites() {
    // Following asserts use appropriate variable rewrites
    assert_erl!(
        r#"pub fn go() {
  let assert Ok(y) = Ok(1)
  let assert Ok(y) = Ok(1)
  y
}"#
    );
}

#[test]
fn message() {
    assert_erl!(
        r#"
pub fn unwrap_or_panic(value) {
  let assert Ok(inner) = value as "Oops, there was an error"
  inner
}
"#
    );
}

#[test]
fn variable_message() {
    assert_erl!(
        r#"
pub fn expect(value, message) {
  let assert Ok(inner) = value as message
  inner
}
"#
    );
}

#[test]
fn just_variable() {
    assert_erl!(
        r#"pub fn go() {
  let assert x = Ok(1)
  x
}"#
    );
}

#[test]
fn tuple_pattern() {
    assert_erl!(
        r#"pub fn go() {
  let assert #(a, b, c) = #(1, 2, 3)
  a + b + c
}"#
    );
}

#[test]
fn int_pattern() {
    assert_erl!(
        r#"pub fn go() {
  let assert 1 = 2
}"#
    );
}

#[test]
fn float_pattern() {
    assert_erl!(
        r#"pub fn go() {
  let assert 1.5 = 5.1
}"#
    );
}

#[test]
fn string_pattern() {
    assert_erl!(
        r#"pub fn go() {
  let assert "Hello!" = "Hel" <> "lo!"
}"#
    );
}

#[test]
fn assignment_pattern() {
    assert_erl!(
        r#"pub fn go() {
  let assert 123 as x = 123
  x
}"#
    );
}

#[test]
fn discard_pattern() {
    assert_erl!(
        r#"pub fn go() {
  let assert _ = 123
}"#
    );
}

#[test]
fn list_pattern() {
    assert_erl!(
        r#"pub fn go() {
  let assert [1, x, 3] = [1, 2, 3]
  x
}"#
    );
}

#[test]
fn list_pattern_with_multiple_variables() {
    assert_erl!(
        r#"pub fn go() {
  let assert [a, b, c] = [1, 2, 3]
  a + b + c
}"#
    );
}

#[test]
fn constructor_pattern() {
    assert_erl!(
        r#"pub fn go() {
  let assert Ok(x) = Error(Nil)
  x
}"#
    );
}

#[test]
fn constructor_pattern_with_multiple_variables() {
    assert_erl!(
        r#"
pub type Wibble {
  Wibble(Int, Float)
}

pub fn go() {
  let assert Wibble(x, 2.0 as y) = Wibble(1, 2.0)
  x
}"#
    );
}

#[test]
fn bit_array_pattern() {
    assert_erl!(
        r#"pub fn go() {
  let assert <<a:2, b:3, c:3>> = <<123>>
  a + b + c
}"#
    );
}

#[test]
fn string_prefix_pattern() {
    assert_erl!(
        r#"pub fn go() {
  let assert "Hello " <> name = "Hello John"
  name
}"#
    );
}

#[test]
fn string_prefix_pattern_with_prefix_binding() {
    assert_erl!(
        r#"pub fn go() {
  let assert "Hello " as greeting <> name = "Hello John"
  #(greeting, name)
}"#
    );
}

#[test]
fn let_assert_at_end_of_block() {
    assert_erl!(
        r#"
pub fn go() {
  let result = Ok(10)
  let x = {
    let assert Ok(_) = result
  }
  x
}"#
    );
}

// https://github.com/gleam-lang/gleam/issues/4145
#[test]
fn reference_earlier_segment() {
    assert_erl!(
        "
pub fn main() {
  let assert <<length, bytes:size(length)-unit(8)>> = <<3, 1, 2, 3>>
  bytes
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/3375
#[test]
fn bit_array_assignment_int() {
    assert_erl!(
        "
pub fn main() {
  let assert <<1 as a>> = <<1>>
  a
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/3375
#[test]
fn bit_array_assignment_float() {
    assert_erl!(
        "
pub fn main() {
  let assert <<3.14 as pi:float>> = <<3.14>>
  pi
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/3375
#[test]
fn bit_array_assignment_string() {
    assert_erl!(
        r#"
pub fn main() {
  let assert <<"Hello, world!" as message:utf8>> = <<"Hello, world!">>
  message
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3375
#[test]
fn bit_array_assignment_discard() {
    assert_erl!(
        r#"
pub fn main() {
  let assert <<_ as number>> = <<10>>
  number
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/4924
#[test]
fn let_assert_should_not_use_redefined_variable() {
    assert_erl!(
        r#"
fn split_once(x: String, y: String) -> Result(#(String, String), String) {
    Ok(#(x, y))
}

pub fn main() {
    let string = "Hello, world!"
    let assert Ok(#(prefix, string)) = split_once(string, "\n")
    as { "Failed to split: " <> string }
}
        "#
    );
}
