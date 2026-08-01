<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2020 The Gleam contributors
-->

# Changelog

## Unreleased

### Compiler

- When compiling to JavaScript any case clauses found to be unreachable will no
  longer generate any code.
  ([Jack Programs](https://github.com/jackprogramsjp))

- The compiler now has a specific error message for when trying to use
  procedural operators that do not exist in Gleam, such as `+=` and `++`.
  ([0xda157](https://github.com/0xda157))

### Build tool

### Language server

- The "Generate dynamic decoder" code action is now only offered when the
  `gleam_stdlib` package is available.
  ([0xda157](https://github.com/0xda157))

### Formatter

### Bug fixes

- Fixed a bug where on the JavaScript target a case clause whose guard's top
  level operator was `||` could run for a subject its pattern did not match.
  ([John Downey](https://github.com/jtdowney))

- Fixed a bug where `gleam export erlang-shipment` would add Gleam contributor
  copyright metadata to the generated POSIX entrypoint.
  ([John Downey](https://github.com/jtdowney))
