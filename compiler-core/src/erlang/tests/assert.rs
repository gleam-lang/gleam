use crate::assert_erl;

#[test]
fn one_var() {
    // One var
    assert_erl!(
        r#"pub fn go() {
  assert Ok(y) = Ok(1)
  y
}"#
    );
}

#[test]
fn more_than_one_var() {
    // More vars
    assert_erl!(
        r#"pub fn go(x) {
  assert [1, a, b, c] = x
  [a, b, c]
}"#
    );
}

#[test]
fn pattern_let() {
    // Pattern::Let
    assert_erl!(
        r#"pub fn go(x) {
  assert [1 as a, b, c] = x
  [a, b, c]
}"#
    );
}

#[test]
fn variable_rewrites() {
    // Following asserts use appropriate variable rewrites
    assert_erl!(
        r#"pub fn go() {
  assert Ok(y) = Ok(1)
  assert Ok(y) = Ok(1)
  y
}"#
    );
}

// TODO: patterns that are just vars don't render a case expression
