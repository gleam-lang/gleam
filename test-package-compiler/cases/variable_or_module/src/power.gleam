// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2022 The Gleam contributors

// https://github.com/gleam-lang/gleam/issues/807
pub type Power {
  Power(value: Int)
}

pub fn to_int(p: Power) {
  p.value * 9000
}
