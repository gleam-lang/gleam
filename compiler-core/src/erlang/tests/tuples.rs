// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use crate::assert_erl;

#[test]
fn simple_tuple() {
    assert_erl!(
        "
pub fn main() {
  #(1, 2, False)
}
"
    )
}

#[test]
fn tuple_with_record_update() {
    assert_erl!(
        "
pub type Wibble { Wibble (a: Int, b: Int) }
pub fn main() {
  let base = Wibble(1, 2)
  #(Wibble(..base, a: 2), False)
}
"
    )
}

#[test]
fn tuple_with_pipeline() {
    assert_erl!(
        "
pub fn main(x) {
  #(1 |> wibble |> wibble, False)
}

fn wibble(n) { n }
"
    )
}

#[test]
fn tuple_index() {
    assert_erl!(
        "
pub fn main() {
  let a = #(1, 2, 3)
  a.0
}
"
    )
}

#[test]
fn tuple_index_2() {
    assert_erl!(
        "
pub fn main() {
  #(1, 2, 3).1
}
"
    )
}

#[test]
fn tuple_destructuring() {
    assert_erl!(
        "
pub fn main(tuple) {
  let #(a, _) = tuple
  a
}
"
    )
}
