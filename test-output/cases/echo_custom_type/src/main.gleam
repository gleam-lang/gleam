// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 The Gleam contributors

pub type Wibble {
  Wibble(a: Int, b: String)
  Wobble(a: List(Float))
  Woo
}

pub fn main() {
  echo Wibble(1, "hello")
  echo Wobble([1.0, 1.1])
  echo Woo
}
