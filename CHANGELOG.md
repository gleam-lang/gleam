# Changelog

## Unreleased

### Build tool

- The `--no-print-progress` flag has been added to prevent the build tool from
  printing messages as the project is built.
  ([Ankit Goel](https://github.com/crazymerlyn))

### Compiler

- It is now possible to omit the `:utf8` option for literal strings used in a
  `BitArray` segment.

  ```gleam
  <<"Hello", " ", "world">>
  ```

  Is the same as:

  ```gleam
  <<"Hello":utf8, " ":utf8, "world":utf8>>
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- In inexhaustive pattern match errors the missing variants variants are now
  printed using the correct syntax for the module the error is emitted in,
  rather than the module it was defined in.

  For example, if the variant would need be qualified by the name of the
  defining module then that would be shown. If the variant was aliased when it
  was imported then the alias would be shown.
  ([Surya Rose](https://github.com/gearsdatapacks))

- Improves how inference works for anonymous functions followed by call
  arguments. For example:

  ```gleam
  fn(x) { x.0 }(#(1, 2))
  ```

  would be infered as `fn() -> Int` in this context.
  ([sobolevn](https://github.com/sobolevn))

- Improves how inference works for anonymous functions inside a pipe.
  For example:

  ```gleam
  pub fn main() {
  let a = 1
     |> fn (x) { #(x, x + 1) }
     |> fn (x) { x.0 }
     |> fn (x) { x }
  }
  ```

  Now inferes correctly to return `Int`.
  ([sobolevn](https://github.com/sobolevn))

### Formatter

### Language Server

### Bug Fixes

- Fixed a bug which caused the language server and compiler to crash when
  two constructors of the same name were created.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where jumping to the definition of an unqualified function would
  produce the correct location, but remain in the same file.
  ([Surya Rose](https://github.com/gearsdatapacks))

- Fixed a bug where incorrect syntax error message were shown,
  when using `:` or `=` in wrong possitions in expressions.
  ([Ankit Goel](https://github.com/crazymerlyn))

- Fixed a bug where the compiler would crash when pattern matching on a type
  which had constructors of duplicate names.
  ([Surya Rose](https://github.com/gearsdatapacks))

- Fixed a bug where referencing record constructors in JavaScript constants but
  not calling them could produce invalid code.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where source links in HTML documentation would be incorrect for
  Codeberg, SourceHut, and Gitea.
  ([sobolevn](https://github.com/sobolevn))

- Fixed a bug with Erlang code generation for discard utf8 patterns in bit
  arrays.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.4.1 - 2024-08-04

### Bug Fixes

- Fix a bug that caused record accessors for private types to not be completed
  by the LSP, even when in the same module.
  ([Ameen Radwan](https://github.com/Acepie))
