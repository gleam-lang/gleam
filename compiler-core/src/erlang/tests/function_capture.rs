// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use crate::assert_erl;

#[test]
fn captured_add() {
    assert_erl!(
        r#"
pub fn add(x, y) {
    x + y
}
pub fn main() {
    let add_one = add(_, 1)
    let one = add_one(0)
}"#,);
}

#[test]
fn captured_lambda() {
    assert_erl!(
        r#"
pub fn main() {
  let func = fn() {
    fn(x) { x }
  }
  let f = func()(_)

  f(0)
}"#,);
}

#[test]
fn captured_tuple() {
    assert_erl!(
        r#"
pub fn main() {
  let func = fn() {
    #(Nil, fn(x, y, z) { x + y + z })
  }
  let f = func().1(1, _, 2)

  f(0)
}"#,);
}

#[test]
fn captured_fieldaccess() {
    assert_erl!(
        r#"
pub type A(a) {
  A(f: fn(a) -> a)
}
pub fn main() {
  let func = fn() {
    A(fn(x) { x })
  }
  let f = func().f(_)

  f(0)
}"#,);
}
