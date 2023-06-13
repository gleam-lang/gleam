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

// https://github.com/gleam-lang/gleam/issues/2179
#[test]
fn const_type_variable() {
    assert_erl!(
        r#"
fn identity(a: a) -> a {
  a
}

const id: fn(a) -> a = identity
"#
    );
}
