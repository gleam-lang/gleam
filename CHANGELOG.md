# Changelog

## Unreleased

### Compiler

- The compiler can now suggest to wrap a value in an `Ok` or `Error` if that can
  solve a type mismatch error:

  ```gleam
  pub fn first(list: List(a)) -> Result(a, Nil) {
    case number {
      [] -> Error(Nil)
      [first, ..rest] -> first
    }
  }
  ```

  Results in the following error:

  ```txt
  error: Type mismatch
    ┌─ /src/one/two.gleam:5:5
    │
  5 │     [first, ..rest] -> first
    │     ^^^^^^^^^^^^^^^^^^^^^^^^
    │                        │
    │                        Did you mean to wrap this in an `Ok`?

  This case clause was found to return a different type than the previous
  one, but all case clauses must return the same type.

  Expected type:

      Result(a, Nil)

  Found type:

      a
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

### Language server

### Formatter

### Bug fixes
