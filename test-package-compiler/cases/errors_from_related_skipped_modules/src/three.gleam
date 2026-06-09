// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//// This module depends on two, that doesn't have any errors but it should be
//// skipped anyway as it depends on a module that does have errors.
//// So this module has to be skipped as well!!

import two

pub fn three_error() {
  // This error should pop up as the module should be skipped
  1 + False
}
