<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2020 The Gleam contributors
-->

# Changelog

## Unreleased

### Compiler

- The compiler now issues a friendlier error when attempting to pattern match
  on both the prefix and suffix of a string:

  ```
  error: Syntax error
    ┌─ /src/parse/error.gleam:2:23
    │
  2 │     "prefix" <> infix <> "suffix" -> infix
    │                       ^^^^^^^^^^^ This pattern is not allowed

  A string pattern can only match on a literal string prefix.

  Matching on a literal suffix is not possible, because `infix` would have an
  unknown size.
  ```

  ([Gavin Morrow](https://github.com/gavinmorrow))

- The compiler now has a specific error message for when trying to use
  procedural operators that do not exist in Gleam, such as `+=` and `++`.
  ([0xda157](https://github.com/0xda157))

### Build tool

### Language server

- The "Generate dynamic decoder" code action is now only offered when the
  `gleam_stdlib` package is available.
  ([0xda157](https://github.com/0xda157))

- The language server now supports triggering "Convert int to different base"
  code action in constants, patterns and bit array "size" options.
  ([Andrey Kozhev](https://github.com/ankddev))

### Formatter

### Bug fixes

- Fixed a bug where on the JavaScript target a case clause whose guard's top
  level operator was `||` could run for a subject its pattern did not match.
  ([John Downey](https://github.com/jtdowney))

- Fixed a bug where `gleam export erlang-shipment` would add Gleam contributor
  copyright metadata to the generated POSIX entrypoint.
  ([John Downey](https://github.com/jtdowney))
