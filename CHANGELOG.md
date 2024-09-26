# Changelog

## Unreleased

### Build tool

### Compiler

### Formatter

### Language Server

- The Language Server now displays correctly qualified or aliased type names
  when hovering over a value in a Gleam file:

  ```gleam
  import gleam/option

  const value = option.Some(1)
  //    ^ hovering here shows `option.Option(Int)`
  ```

  ```gleam
  import gleam/option.{type Option as Maybe}

  const value = option.Some(1)
  //    ^ hovering here shows `Maybe(Int)`
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

### Bug Fixes

- Fixed a bug where the formatter would not format strings with big grapheme
  clusters properly.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.5.1 - 2024-09-26

### Bug Fixes

- Fixed a bug where Erlang file paths would not be escaped on Windows.
  ([Louis Pilfold](https://github.com/lpil))
