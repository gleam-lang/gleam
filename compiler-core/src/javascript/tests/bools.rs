use crate::assert_js;

#[test]
fn expressions() {
    assert_js!(
        r#"
fn go() {
    True
    False
    Nil
}
"#
    );
}

#[test]
fn constants() {
    assert_js!(
        r#"
const a = True
const b = False
const c = Nil
"#,
    );
}

#[test]
fn operators() {
    assert_js!(
        r#"
fn go() {
    True && True
    False || False
}
"#,
    );
}

#[test]
fn assigning() {
    assert_js!(
        r#"
fn go(x, y) {
  assert True = x
  assert False = x
  assert Nil = y
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1112
// differentiate between prelude constructors and custom type constructors
#[test]
fn shadowed_bools_and_nil() {
    assert_js!(
        r#"
pub type True { True False Nil }
fn go(x, y) {
  assert True = x
  assert False = x
  assert Nil = y
}
"#,
    );
}

#[test]
fn equality() {
    assert_js!(
        r#"
fn go(a, b) {
  a == True
  a != True
  a == False
  a != False
  a == a
  a != a
  b == Nil
  b != Nil
  b == b
}
"#,
    );
}

#[test]
fn case() {
    assert_js!(
        r#"
fn go(a) {
  case a {
    True -> 1
    False -> 0
  }
}
"#,
    );
}

#[test]
fn nil_case() {
    assert_js!(
        r#"
fn go(a) {
  case a {
    Nil -> 0
  }
}
"#,
    );
}

#[test]
fn negation() {
    assert_js!(
        "pub fn negate(x) {
    !x
}"
    );
}

#[test]
fn negation_block() {
    assert_js!(
        "pub fn negate(x) {
  !{
    123
    x
  }
}"
    );
}
