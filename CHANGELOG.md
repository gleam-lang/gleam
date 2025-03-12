# Changelog

## Unreleased

### Compiler

- On the JavaScript target, bit arrays can now use the `unit` option to control
  the units of the `size` option.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Build tool

### Language server

### Formatter

### Bug fixes

- Fixed a bug where tuples with atoms in the first position could be
  incorrectly formatted by `echo`.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where unlabelled arguments would be allowed after labelled
  arguments in variant constructor definitions.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where using the "Convert to pipe" code action on a function whose
  first argument is itself a pipe would result in invalid code.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.9.1 - 2025-03-10

### Formatter

- Improved the formatting of pipelines printed with `echo`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug fixes

- Fixed a bug where `echo` used before a pipeline would generate invalid code
  for the Erlang target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
