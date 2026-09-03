// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

pub fn main() -> Nil {
  print("Hello, Joe!\n")
  Nil
}

@external(erlang, "io", "format")
fn print(string: String) -> Atom

type Atom
