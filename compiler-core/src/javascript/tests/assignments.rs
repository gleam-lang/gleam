use crate::assert_js;

#[test]
fn tuple_matching() {
    assert_js!(
        r#"
fn go(x) {
  let #(1, 2) = x
}
"#,
    )
}

#[test]
fn assert() {
    assert_js!(r#"fn go(x) { assert 1 = x }"#,);

    assert_js!(r#"fn go(x) { assert #(1, 2) = x }"#,);
}

#[test]
fn nested_binding() {
    assert_js!(
        r#"
fn go(x) {
  let #(a, #(b, c, 2) as t, _, 1) = x
}
"#,
    )
}

#[test]
fn variable_renaming() {
    assert_js!(
        r#"

fn go(x, foo) {
  let a = 1
  foo(a)
  let a = 2
  foo(a)
  let #(a, 3) = x
  let b = a
  foo(b)
  let c = {
    let a = a
    #(a, b)
  }
  foo(a)
  // make sure arguments are counted in initial state
  let x = c
  x
}
"#,
    )
}

#[test]
fn constant_assignments() {
    assert_js!(
        r#"
const a = True

fn go() {
  a
  let a = 10
  a + 20
}

fn second() {
  let a = 10
  a + 20
}
"#,
    );
}

#[test]
fn returning_literal_subject() {
    assert_js!(r#"fn go(x) { assert 1 = x + 1 }"#,);
}

#[test]
fn rebound_argument() {
    assert_js!(
        r#"pub fn main(x) {
  let x = False
  x
}
"#,
    );
}

#[test]
fn rebound_function() {
    assert_js!(
        r#"pub fn x() { 
  Nil
}
        
pub fn main() {
  let x = False
  x
}
"#,
    );
}

#[test]
fn rebound_function_and_arg() {
    assert_js!(
        r#"pub fn x() { 
  Nil
}
        
pub fn main(x) {
  let x = False
  x
}
"#,
    );
}

#[test]
fn variable_used_in_pattern_and_assignment() {
    assert_js!(
        r#"pub fn main(x) {
  let #(x) = #(x)
  x
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1253
#[test]
fn correct_variable_renaming_in_assigned_functions() {
    assert_js!(
        r#"
pub fn debug(x) {
  let x = x
  fn(x) { x + 1 }
}
"#,
    );
}
