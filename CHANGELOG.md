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

## v1.9.1 - 2025-03-10

### Formatter

- Improved the formatting of pipelines printed with `echo`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug fixes

- Fixed a bug where `echo` used before a pipeline would generate invalid code
  for the Erlang target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
