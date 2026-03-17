# Changelog

## Unreleased

### Compiler

### Build tool

### Language server

### Formatter

### Bug fixes

- Fixed a bug where `BitArray$BitArray$data` constructed a `DataView` with
  incorrect byte length instead of the slice's actual size, causing sliced bit
  arrays to include extra bytes from the underlying buffer on JavaScript.
  ([John Downey](https://github.com/jtdowney))

## v1.15.1 - 2026-03-17

### Bug fixes

- Fixed a bug where `BitArray$BitArray$data` constructed a `DataView` with
  offset 0 instead of the slice's actual byte offset, causing sliced bit arrays
  to read from the wrong position in the underlying buffer on JavaScript.
  ([John Downey](https://github.com/jtdowney))
