// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

pub type Box(a, b, c) {
  AlmostFull(a, c)
  AlmostEmpty(b)
  Empty
}
