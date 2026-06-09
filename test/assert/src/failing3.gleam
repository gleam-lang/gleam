// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

import gleam/bool
import gleam/result

pub fn main() {
  assert bool.negate(False) && result.is_ok(Error(81))
}
