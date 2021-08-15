use crate::assert_js;

#[test]
fn top() {
    assert_js!(
        r#"pub fn main(x) {
  try y = x
  try z = y
  Ok(z)
}"#,
    )
}

#[test]
fn rebinding() {
    assert_js!(
        r#"pub fn main(x) {
  try x = x
  try x = x
  Ok(x)
}"#,
    )
}

#[test]
fn discard() {
    assert_js!(
        r#"pub fn main(x, y) {
  try _ = x
  try _ = y
  x
}"#,
    )
}

#[test]
fn with_subpattern() {
    assert_js!(
        r#"pub fn main(x) {
  try #(a, b) = x
  try #(1, 2) = x
  try #(a, 2) = Ok(#(1, 2))
  Ok(x)
}"#,
    )
}

#[test]
fn in_block() {
    assert_js!(
        r#"pub fn main(x) {
  let y = {
    try z = x
    Ok(z + 1)
  }
  y
}"#,
    )
}

#[test]
fn assert_in_block() {
    assert_js!(
        r#"pub fn main(x) {
  assert Ok(y) = {
    try z = x
    Ok(z + 1)
  }
  y
}"#,
    )
}
