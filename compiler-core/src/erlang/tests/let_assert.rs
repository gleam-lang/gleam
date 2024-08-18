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
fn let_assert_as() {
    assert_erl!(
        r#"pub fn main() {
  let msg = "custom" <> " error"
  let assert as msg Ok(y) = Ok(1)
  y
}"#
    );
}

#[test]
fn let_assert_as_no_var() {
    assert_erl!(
        r#"pub fn main() {
  let assert as "custom error" Ok(y) = Ok(1)
  y
}"#
    );
}

// TODO: patterns that are just vars don't render a case expression
// #[test]
// fn just_pattern() {
//     assert_erl!(
//         r#"pub fn go() {
//   let assert x = Ok(1)
//   x
// }"#
//     );
// }
