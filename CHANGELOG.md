# Changelog

## Unreleased

### Compiler

- Type inference now preserves generic type parameters when constructors or functions are used without explicit annotations, 
  eliminating false errors in mutually recursive code:

  ```gleam
  type Test(a) {
    Test(a)
  }

  fn it(value: Test(a)) {
    it2(value)
  }

  fn it2(value: Test(a)) -> Test(a) {
    it(value)
  }
  ```

  Previously this could fail with an incorrect "type mismatch" error. ([Adi Salimgereyev](https://github.com/abs0luty))

### Build tool

### Language server

- The "extract variable" code action can now pick better names for variables in
  case branches and blocks, ignoring unrelated names of variables in other
  branches.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

### Bug fixes

## v1.15.1 - 2026-03-17

### Bug fixes

- Fixed a bug where `BitArray$BitArray$data` constructed a `DataView` with
  offset 0 instead of the slice's actual byte offset, causing sliced bit arrays
  to read from the wrong position in the underlying buffer on JavaScript.
  ([John Downey](https://github.com/jtdowney))
