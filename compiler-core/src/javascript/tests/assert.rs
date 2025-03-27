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
fn assert_function_call3() {
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
