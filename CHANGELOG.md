<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2020 The Gleam contributors
-->

# Changelog

## v1.19.2 - 2026-10-12

### Bug fixes

- Fixed a bug where the build tool would not reject invalid application names
  if one is returned by the Hex API.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where the compiler would generate invalid Erlang code for user
  defined types, causing external tools like Dyalizer to crash.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.19.1 - 2026-10-07

### Bug fixes

- Fixed a bug where the `export javascript-prelude` and
  `export typescript-prelude` commands would not run.
  ([Louis Pilfold](https://github.com/lpil))

