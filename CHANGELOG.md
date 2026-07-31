<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2020 The Gleam contributors
-->

# Changelog

## Unreleased

### Compiler

### Build tool

### Language server

### Formatter

### Bug fixes

- Fixed a bug where on the JavaScript target a case clause whose guard's top
  level operator was `||` could run for a subject its pattern did not match.
  ([John Downey](https://github.com/jtdowney))
