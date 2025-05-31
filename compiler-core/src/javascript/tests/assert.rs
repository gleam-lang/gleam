use crate::assert_js;

#[test]
fn assert_variable() {
    assert_js!(
        "
pub fn main() {
  let x = True
  assert x
}
"
    );
}

#[test]
fn assert_literal() {
    assert_js!(
        "
pub fn main() {
  assert False
}
"
    );
}

#[test]
fn assert_binary_operation() {
    assert_js!(
        "
pub fn main() {
  let x = True
  assert x || False
}
"
    );
}

#[test]
fn assert_binary_operation2() {
    assert_js!(
        "
pub fn eq(a, b) {
  assert a == b
}
"
    );
}

#[test]
fn assert_binary_operation3() {
    assert_js!(
        "
pub fn assert_answer(x) {
  assert x == 42
}
"
    );
}

#[test]
fn assert_function_call() {
    assert_js!(
        "
fn bool() {
  True
}

pub fn main() {
  assert bool()
}
"
    );
}

#[test]
fn assert_function_call2() {
    assert_js!(
        "
fn and(a, b) {
  a && b
}

pub fn go(x) {
  assert and(True, x)
}
"
    );
}

#[test]
fn assert_nested_function_call() {
    assert_js!(
        "
fn and(x, y) {
  x && y
}

pub fn main() {
  assert and(and(True, False), True)
}
"
    );
}

#[test]
fn assert_binary_operator_with_side_effects() {
    assert_js!(
        "
fn wibble(a, b) {
  let result = a + b
  result == 10
}

pub fn go(x) {
  assert True && wibble(1, 4)
}
"
    );
}

#[test]
fn assert_binary_operator_with_side_effects2() {
    assert_js!(
        "
fn wibble(a, b) {
  let result = a + b
  result == 10
}

pub fn go(x) {
  assert wibble(5, 5) && wibble(4, 6)
}
"
    );
}

#[test]
fn assert_with_message() {
    assert_js!(
        r#"
pub fn main() {
  assert True as "This shouldn't fail"
}
"#
    );
}

#[test]
fn assert_with_block_message() {
    assert_js!(
        r#"
fn identity(a) {
  a
}

pub fn main() {
  assert identity(True) as {
    let message = identity("This shouldn't fail")
    message
  }
}
"#
    );
}

#[test]
fn assert_nil_always_throws() {
    assert_js!(
        r#"
pub fn go(x: Nil) {
  let assert Nil = x
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/4643
#[test]
fn assert_with_pipe_on_right() {
    assert_js!(
        "
fn add(a, b) { a + b }

pub fn main() {
  assert 3 == 1 |> add(2)
}
"
    );
}
