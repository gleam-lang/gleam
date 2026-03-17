# Changelog

## Unreleased

### Compiler

### Build tool

### Language server

### Formatter

### Bug fixes

- Fixed a bug where `BitArray$BitArray$data` constructed a `DataView` with
  offset 0 instead of the slice's actual byte offset, causing sliced bit arrays
  to read from the wrong position in the underlying buffer on JavaScript.
  ([John Downey](https://github.com/jtdowney))
