# Changelog

## Unreleased

### Compiler

- The compiler now reports an error when integer and float binary operators are used 
  incorrectly in case expression guards. ([Adi Salimgereyev](https://github.com/abs0luty))

- The compiler now supports string concatenation in clause guards:

  ```gleam
  case message {
    #(version, action) if version <> ":" <> action == "v1:delete" -> handle_delete()
    _ -> ignore()
  }
  ```

  ([Adi Salimgereyev](https://github.com/abs0luty))

### Build tool

- When adding a package that does not exist on Hex, the message is a bit friendlier.
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

- The compiler now correctly tracks the minimum required version for expressions
  in `BitArray`s' `size` option to be `>= 1.12.0`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
