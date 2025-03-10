# Changelog

## Unreleased

### Compiler

### Build tool

### Language server

### Formatter

### Bug fixes

- Fixed a bug where tuples with atoms in the first position could be
  incorrectly formatted by `echo`.

## v1.9.1 - 2025-03-10

### Formatter

- Improved the formatting of pipelines printed with `echo`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug fixes

- Fixed a bug where `echo` used before a pipeline would generate invalid code
  for the Erlang target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
