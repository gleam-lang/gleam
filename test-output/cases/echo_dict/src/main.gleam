// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 The Gleam contributors

import gleam/dict

pub fn main() {
  echo dict.new()
  echo dict.from_list([#(1, "hello"), #(2, "world!")])
}
