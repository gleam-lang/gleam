// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2022 The Gleam contributors

use crate::assert_ts_def;

// https://github.com/gleam-lang/gleam/issues/5715
#[test]
fn typescript_is_variant_function_has_overload_for_values_with_known_type() {
    assert_ts_def!(
        "
pub type Wibble(a, b, c) {
    Wobble(a, c)
    Woo(b)
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/5715
#[test]
fn typescript_is_variant_function_has_overload_for_values_with_known_type_2() {
    assert_ts_def!(
        "
pub type Box(value) {
    Full(value)
    Empty
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/5715
#[test]
fn typescript_is_variant_function_overload_is_not_added_to_functions_with_no_generics() {
    assert_ts_def!(
        "
pub type Twin {
    Jak
    Tom
}
"
    );
}
