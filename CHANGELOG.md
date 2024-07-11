# Changelog

## Unreleased

### Build tool

### Compiler

### Formatter

### Language Server

### Bug Fixes

## v1.3.2 - 2024-07-11

### Language Server

- The language server no longer shows completions when inside a literal string.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug Fixes

- Fixed a bug where the compiler would report errors for duplicate `@external`
  attributes with inconsistent spans between Erlang and JavaScript.
  ([Connor Szczepaniak](https://github.com/cszczepaniak))

- Fixed a bug where `gleam add` would fail to parse version specifiers
  correctly.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where single clause case expressions could generate JavaScript
  code with incorrectly rewritten JavaScript variable names.
  ([Louis Pilfold](https://github.com/lpil))

## v1.3.1 - 2024-07-10

### Bug Fixes

- Fixes a bug with import cycle detection when there is more than 2 imports in
  the cycle.
  ([Ameen Radwan](https://github.com/Acepie))
