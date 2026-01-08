# Changelog

## Unreleased

### Compiler

### Build tool

- When adding a package that does not exist on Hex, the message is a bit
  friendlier.
  ([Ameen Radwan](https://github.com/Acepie))

### Language server

- The language server now allows extracting the start of a pipeline into a
  variable.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

### Bug fixes

- The compiler now correctly tracks the minimum required version for constant
  record updates to be `>= 1.14.0`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now correctly tracks the minimum required version for
  expressions in `BitArray`s' `size` option to be `>= 1.12.0`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the formatter would not properly format some function calls
  if the last argument was followed by a trailing comment.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server no longer recommends the deprecated `@target` attribute.
  ([Hari Mohan](https://github.com/seafoamteal))

- The compiler no longer crashes when trying to pattern match on a `UtfCodepoint`.
  ([Hari Mohan](https://github.com/seafoamteal))
