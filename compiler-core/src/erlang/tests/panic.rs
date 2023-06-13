use crate::assert_erl;

#[test]
fn plain() {
    assert_erl!(
        r#"
pub fn main() {
  panic
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2176
#[test]
fn piped() {
    assert_erl!(
        r#"
pub fn main() {
  "lets"
  |> panic
}
    "#
    );
}
