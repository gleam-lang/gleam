// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2020 The Gleam contributors

pub type Box(a) {
  Box(tag: String, value: a)
}
