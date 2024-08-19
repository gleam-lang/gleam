# Changelog

## Unreleased

### Build tool

- The `--no-print-progress` flag has been added to prevent the build tool from
  printing messages as the project is built.
  ([Ankit Goel](https://github.com/crazymerlyn))

### Compiler

- Compiler progress is now printed to stderr, instead of stdout.
  ([Victor Kobinski](https://github.com/vkobinski))

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

- Anonymous functions that are immediately called with a record or a tuple as an
  argument are now inferred correctly without the need to add type annotations.
  For example you can now write:

  ```gleam
  fn(x) { x.0 }(#(1, 2))
  // ^ you no lonfer need to annotate this!
  ```

  ([sobolevn](https://github.com/sobolevn))

- Anonymous functions that are being piped a record or a tuple as an argument
  are now inferred correctly without the need to add type annotations. For
  example you can now write:

  ```gleam
  pub type User {
    User(name: String)
  }

  pub fn main() {
    User("Giacomo")
    |> fn(user) { user.name }
    //    ^^^^ you no longer need to annotate this!
    |> io.debug
  }
  ```

  ([sobolevn](https://github.com/sobolevn))

- The record pattern matching syntax `Record(a ..)` is now deprecated in favour
  of the `Record(a, ..)` syntax.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Adds a better error message when module names are used as values. For example
  the following code:

  ```gleam
  import gleam/list

  pub fn main() {
    list
  }
  ```

  Results in the error:

  ```txt
  error: Module `list` used as a value
    ┌─ /Users/giacomocavalieri/Desktop/prova/src/prova.gleam:4:3
    │
  4 │   list
    │   ^^^^

  Modules are not values, so you cannot assign them to variables, pass them to
  functions, or anything else that you would do with a value.
  ```

  ([sobolevn](https://github.com/sobolevn))

- Adds a hint to syntax error when defining named function inside another
  function.
  ([sobolevn](https://github.com/sobolevn))

### Formatter

### Language Server

### Bug Fixes

- Fixed a bug which caused the language server and compiler to crash when two
  constructors of the same name were created.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where jumping to the definition of an unqualified function would
  produce the correct location, but remain in the same file.
  ([Surya Rose](https://github.com/gearsdatapacks))

- Fixed a bug where incorrect syntax error message were shown, when using `:` or
  `=` in wrong possitions in expressions.
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

- Fixed a bug which affected inference of function calls in pipe expressions.
  ([sobolevn](https://github.com/sobolevn))

## v1.4.1 - 2024-08-04

### Bug Fixes

- Fix a bug that caused record accessors for private types to not be completed
  by the LSP, even when in the same module.
  ([Ameen Radwan](https://github.com/Acepie))
