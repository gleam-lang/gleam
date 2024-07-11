# Changelog

## Unreleased

### Build tool

### Compiler

### Formatter

### Language Server

### Bug Fixes

- Fixed a bug where the compiler would report errors for duplicate `@external`
  attributes with inconsistent spans between Erlang and JavaScript.
  ([Connor Szczepaniak](https://github.com/cszczepaniak))

## v1.3.1 - 2024-07-10

### Bug Fixes

- Fixes a bug with import cycle detection when there is more than 2 imports in the cycle
  ([Ameen Radwan](https://github.com/Acepie))
