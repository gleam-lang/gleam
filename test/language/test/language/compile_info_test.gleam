// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import gleam/string

@target(erlang)
@external(erlang, "ffi_erlang", "module_source")
fn module_source(module: String) -> String

// The BEAM compile info should point at the original Gleam source so tools such
// as debuggers can find it.
@target(erlang)
pub fn source_is_gleam_file_test() {
  assert string.ends_with(module_source("language"), "/src/language.gleam")
}
