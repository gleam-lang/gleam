use crate::assert_erl;

#[test]
fn numbers_with_underscores() {
    assert_erl!(
        r#"
pub fn main() {
  100_000
  100_000.00101
}
"#
    );

    assert_erl!(
        r#"
const i = 100_000
const f = 100_000.00101
pub fn main() {
  i
  f
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  let 100_000 = 1
  let 100_000.00101 = 1.
  1
}
"#
    );
}
