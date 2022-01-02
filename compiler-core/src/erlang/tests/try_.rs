use crate::assert_erl;

#[test]
fn try_expr() {
    assert_erl!(
        r#"
pub fn main() {
    try a = Ok(1)
    try b = Ok(2)
    Ok(a + b)
}
"#
    );
}
#[test]
fn try_in_case_subject() {
    assert_erl!(
        "pub fn x(f) {
  try x = 1 |> f
  Ok(x)
}"
    );
}
