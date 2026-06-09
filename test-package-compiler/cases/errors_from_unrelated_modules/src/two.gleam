// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import one

pub fn two_error() {
  // This module depends on `one` which itself has an error.
  // So this should not result in an error!!
  1 + False
}
