// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

pub type Box {
  Box(constructor: Int)
}

pub fn record_with_constructor_as_field_can_be_built_on_js_with_no_crash_test() {
  let one = Box(1)
  let other = Box(1)
  assert one == other
}
