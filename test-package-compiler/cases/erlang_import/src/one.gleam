// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2022 The Gleam contributors

import two

pub fn unbox(x) {
  let two.Box(i) = x
  i
}
