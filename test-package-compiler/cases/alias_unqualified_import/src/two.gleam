// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2022 The Gleam contributors

// https://github.com/gleam-lang/gleam/issues/303
import one.{Empty as E, id as i}

pub fn make() {
  i(E)
}
