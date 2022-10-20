use crate::assert_erl;

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
