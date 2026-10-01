// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

pub type A(a) {
  A(f: fn(a) -> a)
}

/// This test ensures that for a function capture like `<expr>(_)`, `<expr>` is evaluated eagerly.
/// For example, having the compiler naively rewrite it to `fn(x) { <expr>(x) }` is incorrect.
/// Related issue https://github.com/gleam-lang/gleam/issues/6242
pub fn main() {
  echo "`<expr>` should be evaluated eagerly"
  let func = fn() {
    echo "eagerly 0"
    fn(x) { x }
  }
  let f0 = func()(_)

  let func = fn() {
    echo "eagerly 1"
    A(fn(x) { x })
  }
  let f1 = func().f(_)

  let func = fn() {
    echo "eagerly 2"
    #(Nil, fn(x) { x })
  }
  let f2 = func().1(_)

  echo "The function call below should not run echo."
  f0(0)
  f1(0)
  f2(0)
}
