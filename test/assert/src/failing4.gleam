// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

import gleam/int

pub fn main() {
  assert int.add(4, 5) > int.absolute_value(-11)
}
