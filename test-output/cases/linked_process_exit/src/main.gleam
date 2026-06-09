// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

@external(erlang, "main_ffi", "spawn_and_exit")
fn spawn_and_exit() -> Nil

pub fn main() {
  spawn_and_exit()
}
