import module_two

// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

pub fn panics_on_line_9() -> Nil {
  // We call a function from another module so it won't be inlined and we'll see
  // some lines in the stack trace!
  module_two.panics_on_line_5()
  // We return Nil so the call above won't be tail call optimised, deleting the
  // stack frame from the stack trace.
  Nil
}
