// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import module_one

pub fn main() {
  some_function()
}

fn some_function() {
  // We call a function from another module so it won't be inlined and we'll see
  // some lines in the stack trace!
  module_one.panics_on_line_9()
  Nil
}
