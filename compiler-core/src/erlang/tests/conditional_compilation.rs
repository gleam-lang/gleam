use crate::assert_erl;

#[test]
fn excluded_attribute_syntax() {
    assert_erl!(
        "@target(javascript)
  pub fn main() { 1 }
"
    );
}

#[test]
fn included_attribute_syntax() {
    assert_erl!(
        "@target(erlang)
  pub fn main() { 1 }
"
    );
}
