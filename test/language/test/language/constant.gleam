//// This module defines constants that can be imported to check imported
//// constants are handled correctly by the compiler.
////

// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

pub const a_string = "hello"

pub const a_number = 11

pub const a_value = Wibble(11)

pub const a_constructor = Wibble

pub type Wibble {
  Wibble(Int)
}
