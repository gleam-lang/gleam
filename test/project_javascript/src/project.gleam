// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

pub fn main() {
  println("Hello, from project_javascript!")
}

@external(erlang, "erlang", "display")
@external(javascript, "./project_ffi.mjs", "log")
fn println(a: String) -> Nil
