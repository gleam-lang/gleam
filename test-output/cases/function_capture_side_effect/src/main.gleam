// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

pub type A(a) {
  A(f: fn(a) -> a)
}

pub fn main() {
  let func = fn() {
    echo "hello"
    fn(x) { x }
  }
  // UntypedExpr::Call
  let _v = func()(_)

  let func = fn() {
    echo "hello"
    A(fn(x) { x })
  }
  // UntypedExpr::FieldAccess
  let _v = func().f(_)

  let func = fn() {
    echo "hello"
    #(Nil, fn(x) { x })
  }
  // UntypedExpr::TupleIndex
  let _v = func().1(_)
}
