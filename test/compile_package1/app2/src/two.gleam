// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

import one
import one/nested

pub fn main() {
  one.hello()
  |> nested.id
}
