# Changelog

## Unreleased

### Compiler

- Optimised code generated for record updates.
  ([yoshi](https://github.com/joshi-monster))

- The compiler now allows for record updates to change the generic type
  parameters of the record:

  ```gleam
  type Box(value) {
    Box(password: String, value: value)
  }

  fn insert(box: Box(a), value: b) -> Box(b) {
    Box(..box, value:)
  }
  ```

  ([yoshi](https://github.com/joshi-monster))

### Build tool

- Improved the error message you get when trying to add a package that doesn't
  exist with `gleam add`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Language Server

- The language server now provides type information when hovering over argument
  labels.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Formatter

### Bug fixed

## v1.6.1 - 2024-11-19

### Bug fixed

- Fixed a bug where `gleam update` would fail to update versions.
