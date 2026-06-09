// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

pub fn main() {
  let message = "message"

  echo 1 as "message1"

  1
  |> echo as { message <> "2" }
  |> fn(n) { n + 1 }
  |> echo as "message3"
}
