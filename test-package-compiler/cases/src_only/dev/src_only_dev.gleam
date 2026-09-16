// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2022 The Gleam contributors

pub fn main() {
  // This package is compiled with the src-only argument, and
  // this type error is to ensure that this directory is not
  // compiled by mistake.
  1 + "deliberate type error"
}
