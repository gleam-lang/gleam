use crate::assert_erl;

#[test]
fn int_add() {
    assert_erl!("pub fn main() { 1 + 2 }")
}

#[test]
fn int_adds() {
    assert_erl!("pub fn main(a, b) { 1 + 2 + a + b }")
}

#[test]
fn int_sub() {
    assert_erl!("pub fn main() { 1 - 2 }")
}

#[test]
fn int_subs() {
    assert_erl!("pub fn main(a) { 1 - 2 - a }")
}

#[test]
fn int_mult() {
    assert_erl!("pub fn main() { 1 * 2 }")
}

#[test]
fn int_mults() {
    assert_erl!("pub fn main(a, b) { 1 * 2 * a * b }")
}
