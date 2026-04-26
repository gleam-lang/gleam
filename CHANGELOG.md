# Changelog

## Unreleased

### Compiler

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

- Fixed a bug where the compiler wouldn't track the minimum required version
  when using list prepending in constants.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server wouldn't let one extract record
  constructors as variables.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
