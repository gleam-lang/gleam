// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

pub type Location {
  Location(
    href: String,
    origin: String,
    protocol: String,
    host: String,
    hostname: String,
    port: String,
    pathname: String,
    search: String,
    hash: String,
  )
}

pub fn main() {
  location()
}

@external(javascript, "./project_ffi.mjs", "location")
fn location() -> Location
