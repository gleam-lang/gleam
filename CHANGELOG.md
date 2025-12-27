# Changelog

## Unreleased

- The language server now offers code actions to wrap a function reference in an
  anonymous function, or to remove a trivial anonymous function, leaving its
  contents. For example:

  ```gleam
  pub fn main() {
    [-1, -2, -3] |> list.map(fn(a) { int.absolute_value(a) })
                          // ^^ Activating the "Remove anonymous function"
                          // code action here
  }
  ```

  would result in:

  ```gleam
  pub fn main() {
    [-1, -2, -3] |> list.map(int.absolute_value)
  }
  ```

  while the other action would reverse the change. ([Eli Treuherz](http.github.com/treuherz))

### Compiler

### Build tool

### Language server

### Formatter

### Bug fixes

- The compiler now correctly tracks the minimum required version for constant
  record updates to be `>= 1.14.0`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now correctly tracks the minimum required version for expressions
  in `BitArray`s' `size` option to be `>= 1.12.0`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
