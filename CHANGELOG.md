# Changelog

## v1.3.0 - Unreleased

### Build tool

### Compiler

- The compiler now emits a warning for redundant function captures in a
  pipeline:

  ```
  warning: Redundant function capture
    ┌─ /src/warning/wrn.gleam:5:17
    │
  5 │     1 |> wibble(_, 2) |> wibble(2)
    │                 ^ You can safely remove this

  This function capture is redundant since the value is already piped as the
  first argument of this call.

  See: https://tour.gleam.run/functions/pipelines/
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

### Language Server

### Bug Fixes

- Fixed a bug where the compiler would output a confusing error message when
  trying to use the spread syntax to append to a list.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the formatter would strip empty lines out of the body of an
  anonymous function passed as an argument.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.2.1 - 2024-05-30

### Bug Fixes

- Fixed a bug where the compiler could fail to detect modules that would clash
  with Erlang modules.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where dependency version resolution could crash for certain
  release candidate versions.
  ([Marshall Bowers](https://github.com/maxdeviant))

- Fixed a bug where trailing comments would be moved out of a bit array.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
