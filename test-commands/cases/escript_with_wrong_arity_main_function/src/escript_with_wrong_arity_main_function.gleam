// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

// main must not take an argument
pub fn main(stuff: List(String)) -> Nil {
  echo stuff
  Nil
}
