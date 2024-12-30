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
}

#[test]
fn numbers_with_underscores1() {
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
}

#[test]
fn numbers_with_underscores2() {
    assert_erl!(
        r#"
pub fn main() {
  let assert 100_000 = 1
  let assert 100_000.00101 = 1.
  1
}
"#
    );
}

#[test]
fn numbers_with_scientific_notation() {
    assert_erl!(
        r#"
const i = 100.001e223
const j = -100.001e-223

pub fn main() {
  i
  j
}
"#
    );
}

#[test]
fn int_negation() {
    assert_erl!(
        r#"
pub fn main() {
  let a = 3
  let b = -a
}
"#
    );
}

#[test]
fn repeated_int_negation() {
    assert_erl!(
        r#"
pub fn main() {
  let a = 3
  let b = --a
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2356
#[test]
fn zero_b_in_hex() {
    assert_erl!(
        r#"
pub fn main() {
  0xffe0bb
}
"#
    );
}
