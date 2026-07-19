// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

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

#[test]
fn int_divide_by_zero() {
    assert_erl!("pub fn main(a) { a / 0 }")
}

#[test]
fn int_divide_side_effecting_function_by_zero() {
    assert_erl!("pub fn main(a) { a() / 0 }")
}

#[test]
fn int_divide_by_non_zero() {
    assert_erl!("pub fn main(a) { a() / 3 }")
}

#[test]
fn int_divide_with_no_side_effect() {
    assert_erl!("pub fn main(a, b) { a / b }")
}

#[test]
fn int_divide_with_possible_side_effect() {
    assert_erl!("pub fn main(a, b) { a() / b }")
}

#[test]
fn int_remainder_by_zero() {
    assert_erl!("pub fn main(a) { a % 0 }")
}

#[test]
fn int_remainder_side_effecting_function_by_zero() {
    assert_erl!("pub fn main(a) { a() % 0 }")
}

#[test]
fn int_remainder_by_non_zero() {
    assert_erl!("pub fn main(a) { a() % 3 }")
}

#[test]
fn int_remainder_with_no_side_effect() {
    assert_erl!("pub fn main(a, b) { a % b }")
}

#[test]
fn int_remainder_with_possible_side_effect() {
    assert_erl!("pub fn main(a, b) { a() % b }")
}

#[test]
fn float_divide_by_zero() {
    assert_erl!("pub fn main(a) { a /. 0.0 }")
}

#[test]
fn float_divide_side_effecting_function_by_zero() {
    assert_erl!("pub fn main(a) { a() /. 0.0 }")
}

#[test]
fn float_divide_by_non_zero() {
    assert_erl!("pub fn main(a) { a() /. 3.0 }")
}

#[test]
fn float_divide_with_no_side_effect() {
    assert_erl!("pub fn main(a, b) { a /. b }")
}

#[test]
fn float_divide_with_possible_side_effect() {
    assert_erl!("pub fn main(a, b) { a() /. b }")
}

#[test]
fn int_divide_by_zero_as_argument() {
    assert_erl!("pub fn main(wibble, a) { wibble(a / 0) }")
}

#[test]
fn int_divide_side_effecting_function_by_zero_as_argument() {
    assert_erl!("pub fn main(wibble, a) { wibble(a() / 0) }")
}

#[test]
fn int_divide_by_non_zero_as_argument() {
    assert_erl!("pub fn main(wibble, a) { wibble(a() / 3) }")
}

#[test]
fn int_divide_with_no_side_effect_as_argument() {
    assert_erl!("pub fn main(wibble, a, b) { wibble(a / b) }")
}

#[test]
fn int_divide_with_possible_side_effect_as_argument() {
    assert_erl!("pub fn main(wibble, a, b) { wibble(a() / b) }")
}

#[test]
fn negated_binop_is_parenthesised() {
    assert_erl!("pub fn wibble(a, b) { !{ a || b } }")
}

#[test]
fn negated_binop_is_parenthesised_2() {
    assert_erl!("pub fn wibble(a, b, c) { !{ a && b || c } }")
}
