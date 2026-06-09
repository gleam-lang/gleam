// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

import thing

pub fn main() {
  echo #(1, singleton(), singleton())
  Nil
}

@external(javascript, "./main_ffi.mjs", "singleton")
fn singleton() -> thing.Thing {
  thing.Thing
}
