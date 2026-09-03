// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import argv

pub fn main() -> Nil {
  // Using a dependency package
  assert argv.load().arguments == []

  Nil
}
