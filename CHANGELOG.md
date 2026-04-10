# Changelog

## Unreleased

### Compiler

### Build tool

### Language server

### Formatter

### Bug fixes

## v1.15.3 - 2026-04-11

### Bug fixes

- `gleam.toml` files with invalid dependency names now raise an error
  immediately when the file is parsed.
  ([Louis Pilfold](https://github.com/lpil))

## v1.15.2 - 2026-03-19

### Bug fixes

- Fixed a bug where the "Add missing type parameter" code action could be
  triggered on types that do not exist instead of type variables.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

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
