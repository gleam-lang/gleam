# Changelog

## Unreleased

### Compiler

- It is now possible to use the `todo` keyword in constants, this will result in
  an helpful error message rather than a syntax error.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

- The `gleam dev` command now accepts the `--no-print-progress` flag. When this
  flag is passed, no progress information is printed.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Language server

### Formatter

### Bug fixes

- Fixed a bug where the build tool would check for new major versions of a local
  or git dependency on Hex when running `gleam update`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
