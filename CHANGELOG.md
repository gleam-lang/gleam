# Changelog

## Unreleased

### Compiler

### Build tool

- When adding a package that does not exist on Hex, the message is a bit friendlier.
  ([Ameen Radwan](https://github.com/Acepie))

- Now build tool ignores type variables defined in other functions and types,
  improving hints in errors. For example:

  ```gleam
  pub type Wibble(a, b, c, d, e, f, g) {
    Wibble(value: a)
  }

  pub fn main() {
    let x: Nil = []
  }
  ```

  Previously this code would have emit following error:

  ```text
  error: Type mismatch
    ┌─ C:\Users\user\projects\test_gleam\src\test_gleam.gleam:6:16
    │
  6 │   let x: Nil = []
    │                ^^

  Expected type:

      Nil

  Found type:

      List(h)
  ```

  Now it will emit following error:

  ```text
  error: Type mismatch
    ┌─ C:\Users\user\projects\test_gleam\src\test_gleam.gleam:6:16
    │
  6 │   let x: Nil = []
    │                ^^

  Expected type:

      Nil

  Found type:

      List(a)
  ```

  ([Andrey Kozhev](https://github.com/ankddev))

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
