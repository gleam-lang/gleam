// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

pub type Wrapper {
  Wrapper(function: fn(String) -> String)
}

/// This test ensures that for a function capture like `<expr>(_)`, `<expr>` is evaluated
/// once when the capture is created, rather than every time the captured function is called.
/// Related issue https://github.com/gleam-lang/gleam/issues/6242
pub fn main() {
  capture_function_call()
  capture_record_field_access()
  capture_tuple_index()
}

fn capture_function_call() {
  echo "--- 1. Function call capture ---"
  let make_function = fn() {
    echo "Evaluating function call (should only happen once)"
    fn(message) { echo message }
  }

  let captured = make_function()(_)
  captured("Calling captured function 1st time")
  captured("Calling captured function 2nd time")
}

fn capture_record_field_access() {
  echo "--- 2. Field access capture ---"
  let make_record = fn() {
    echo "Evaluating record creation (should only happen once)"
    Wrapper(function: fn(message) { echo message })
  }

  let captured = make_record().function(_)
  captured("Calling captured function 1st time")
  captured("Calling captured function 2nd time")
}

fn capture_tuple_index() {
  echo "--- 3. Tuple index capture ---"
  let make_tuple = fn() {
    echo "Evaluating tuple creation (should only happen once)"
    #(Nil, fn(message) { echo message })
  }

  let captured = make_tuple().1(_)
  captured("Calling captured function 1st time")
  captured("Calling captured function 2nd time")
}
