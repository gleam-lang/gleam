use crate::assert_erl;

// https://github.com/gleam-lang/gleam/issues/2163
#[test]
fn record_constructor() {
    assert_erl!(
        r#"
pub type X {
  X(Int)
}

pub const z = X

pub fn main() {
  z
}"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2163
#[test]
fn record_constructor_in_tuple() {
    assert_erl!(
        r#"
pub type X {
  X(Int)
}

pub const z = #(X)

pub fn main() {
  z
}"#
    );
}
