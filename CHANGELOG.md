# Changelog

## Unreleased

### Compiler

### Build tool

### Language server

- The language server now offers a code action to generate a missing type
  definition when an unknown type is referenced. The generated stub includes
  the correct number of type parameters.
  ([Daniele Scaratti](https://github.com/lupodevelop))

- The "extract variable" code action can now pick better names for variables in
  case branches and blocks, ignoring unrelated names of variables in other
  branches.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

### Bug fixes

- Fixed a bug where the compiler would crash when trying to read the cache for
  modules containing large constants.
  ([Surya Rose](https://github.com/GearsDatapacks))

## v1.15.1 - 2026-03-17

### Bug fixes

- Fixed a bug where `BitArray$BitArray$data` constructed a `DataView` with
  offset 0 instead of the slice's actual byte offset, causing sliced bit arrays
  to read from the wrong position in the underlying buffer on JavaScript.
  ([John Downey](https://github.com/jtdowney))
