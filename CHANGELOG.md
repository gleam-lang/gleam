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

- New projects are generated using OTP28 on GitHub Actions.
  ([Louis Pilfold](https://github.com/lpil))

- `gleam add` now adds `dependencies` and `dev-dependencies` as tables instead
  of inline tables if they are missing.
  ([Andrey Kozhev](https://github.com/ankddev))

### Language server

### Formatter

### Bug fixes

- Fixed a bug where `echo` could crash on JavaScript if the module contains
  record variants with the same name as some built-in JavaScript objects.
  ([Louis Pilfold](https://github.com/lpil))
