# Changelog

## Unreleased

### Build tool

- `gleam docs build` now takes an optional `--target` flag to specify the target
  platform for the generated documentation.
  ([Jiangda Wang](https://github.com/frank-iii))

### Compiler

- The warning for the deprecated `[..]` pattern has been improved.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Record accessors are now fault tolerant. This means an invalid label can be
  properly detected and won't invalidate the rest of the expression.
  ([Ameen Radwan](https://github.com/Acepie))

- Fix cases where in Erlang unbound type variables are generated.
  ([Damir Vandic](https://github.com/dvic))

- Error messages for invalid record constructors now contain a restructured
  example of what the user likely intended. This is especially helpful for
  users coming from other languages, like Rust or Go.

  For example, provided a User type:

  ```
  pub type User {
    name: String
  }
  ```

  The compiler errors with the following message:

  ```
  error: Syntax error
    ┌─ /src/parse/error.gleam:3:5
    │
  3 │     name: String,
    │     ^^^^ I was not expecting this

  Each custom type variant must have a constructor:

  pub type User {
    User(
      name: String,
    )
  }
  ```

  ([Rahul D. Ghosal](https://github.com/rdghosal))

- The `<>` string concatenation operator can now be used in constant
  expressions.
  ([Thomas](https://github.com/DeviousStoat))

- Function calls are now fault tolerant. This means that errors in the function
  call arguments won't stop the rest of the call from being analysed.
  ([Ameen Radwan](https://github.com/Acepie))

### Formatter

### Language Server

- The language server can now show completions for fields if a record access is
  being attempted.
  ([Ameen Radwan](https://github.com/Acepie))

### Bug Fixes

- Functions, types and constructors named `module_info` are now escaped
  in generated Erlang code to avoid conflicts with the builtin
  `module_info/0` and `module_info/1` functions.
  ([Juraj Petráš](https://github.com/Hackder))

- Fixed formatting of comments at the start of a case branch.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.3.2 - 2024-07-11

### Language Server

- The language server no longer shows completions when inside a literal string.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug Fixes

- Fixed a bug where the compiler would report errors for duplicate `@external`
  attributes with inconsistent spans between Erlang and JavaScript.
  ([Connor Szczepaniak](https://github.com/cszczepaniak))

- Fixed a bug where `gleam add` would fail to parse version specifiers
  correctly.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where single clause case expressions could generate JavaScript
  code with incorrectly rewritten JavaScript variable names.
  ([Louis Pilfold](https://github.com/lpil))

## v1.3.1 - 2024-07-10

### Bug Fixes

- Fixes a bug with import cycle detection when there is more than 2 imports in
  the cycle.
  ([Ameen Radwan](https://github.com/Acepie))
