use crate::assert_erl;

#[test]
fn assert_variable() {
    assert_erl!(
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
    assert_erl!(
        "
pub fn main() {
  assert False
}
"
    );
}

#[test]
fn assert_binary_operation() {
    assert_erl!(
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
    assert_erl!(
        "
pub fn eq(a, b) {
  assert a == b
}
"
    );
}

#[test]
fn assert_binary_operation3() {
    assert_erl!(
        "
pub fn assert_answer(x) {
  assert x == 42
}
"
    );
}

#[test]
fn assert_function_call() {
    assert_erl!(
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
    assert_erl!(
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
    assert_erl!(
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
    assert_erl!(
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
    assert_erl!(
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
    assert_erl!(
        r#"
pub fn main() {
  assert True as "This shouldn't fail"
}
"#
    );
}

#[test]
fn assert_with_block_message() {
    assert_erl!(
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
