use crate::assert_erl;

#[test]
fn excluded() {
    assert_erl!(
        "if javascript {
  pub fn main() { 1 }
}"
    );
}

#[test]
fn included() {
    assert_erl!(
        "if erlang {
  pub fn main() { 1 }
}"
    );
}
