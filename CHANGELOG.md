# Changelog

## Unreleased

### Compiler

- On the JavaScript target, bit arrays can now use the `unit` option to control
  the units of the `size` option.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Build tool

### Language server

- The language server now allows renaming of functions and constants across modules.
  For example:

  ```gleam
  // wibble.gleam
  pub fn wibble() {
    wibble()
  //^ Trigger rename
  }
  // wobble.gleam
  import wibble

  pub fn main() {
    wibble.wibble()
  }
  ```

  Becomes:

  ```gleam
  // wibble.gleam
  pub fn wobble() {
    wobble()
  }
  // wobble.gleam
  import wibble

  pub fn main() {
    wibble.wobble()
  }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

### Formatter

### Bug fixes

- Fixed a bug where tuples with atoms in the first position could be
  incorrectly formatted by `echo`.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where unlabelled arguments would be allowed after labelled arguments
  in variant constructor definitions.
  ([Surya Rose](https://github.com/GearsDatapacks))

## v1.9.1 - 2025-03-10

### Formatter

- Improved the formatting of pipelines printed with `echo`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug fixes

- Fixed a bug where `echo` used before a pipeline would generate invalid code
  for the Erlang target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
