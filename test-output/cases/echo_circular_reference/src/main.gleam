// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

pub fn main() {
  echo circular_reference()
  Nil
}

type Thing

@external(javascript, "./main_ffi.mjs", "circular_reference")
fn circular_reference() -> Thing
