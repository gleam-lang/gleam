# Changelog

## v1.4.0 - 2024-08-02

### Bug Fixes

- Fixed a bug where pipe function arity errors could have an incorrect error
  message.
  ([sobolevn](https://github.com/sobolevn))

- Fixed a bug where the case of type parameters would not be checked.
  ([Surya Rose](https://github.com/gearsdatapacks))

- Fixed a bug where the language server would still show completions when inside
  a comment.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.4.0-rc1 - 2024-07-29

### Build tool

- `gleam docs build` now takes an optional `--target` flag to specify the target
  platform for the generated documentation.
  ([Jiangda Wang](https://github.com/frank-iii))

- Warnings are now emitted each time the project is built, even if the module
  the warnings originated from were loaded from the cache rather than
  recompiling.
  ([Louis Pilfold](https://github.com/lpil))

### Compiler

- Labelled arguments can now use the label shorthand syntax.
  This means that when you're passing a variable as a labelled argument and it
  happens to have the same name as the label, you can omit the variable name:

  ```gleam
  pub fn date(day day: Int, month month: Month, year year: Year) -> Date {
    todo
  }

  pub fn main() {
    let day = 11
    let month = October
    let year = 1998

    date(year:, month:, day:)
    // This is the same as writing
    // date(year: year, month: month, day: day)
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Labelled pattern variables can now use the label shorthand syntax.
  This means that when you're pattern matching on a record constructor and
  binding its labelled fields to variables that happen to have the same name,
  you can omit the variable name:

  ```gleam
  pub type Date
    Date(day: Int, month: Month, year: Year)
  }

  pub fn main() {
    case Date(11, October, 1998) {
      Date(year:, month:, day:) -> todo
      // This is the same as writing
      // Date(year: year, month: month, day: day) -> todo
    }

  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The warning for the deprecated `[..]` pattern has been improved.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Record accessors are now fault tolerant. This means an invalid label can be
  properly detected and won't invalidate the rest of the expression.
  ([Ameen Radwan](https://github.com/Acepie))

- Erlang type spec generation has been improved to avoid new warnings emitted in
  OTP27.
  ([Damir Vandic](https://github.com/dvic))

- Error messages for invalid record constructors now contain a restructured
  example of what the user likely intended. This is especially helpful for
  users coming from other languages, like Rust or Go.

  For example, provided a User type:

  ```gleam
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

- The error message presented when a function is called in a guard has been
  improved.
  ([Thomas](https://github.com/DeviousStoat))

- Case expressions are now fault tolerant. This means an subject, pattern,
  guard, or then body can be properly detected and won't invalidate the rest
  of the expression.
  ([Ameen Radwan](https://github.com/Acepie))

- Documentation comments that come before a regular comment are no longer
  clumped together with the documentation of the following definition.
  Now commenting out a definition won't result in its documentation merging with
  the following one's.

  ```gleam
  /// This doc comment will be ignored!
  // a commented definition
  // fn wibble() {}

  /// Wibble's documentation.
  fn wibble() { todo }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The `little` and `big` endianness options, the `signed` and `unsigned` integer
  options, and sized floats (32-bit and 64-bit), can now be used in bit array
  expressions and patterns on the JavaScript target.
  ([Richard Viney](https://github.com/richard-viney))

- The `utf8` option can now be used with constant strings in bit array patterns
  on the JavaScript target.
  ([Richard Viney](https://github.com/richard-viney))

### Formatter

- The formatter will no longer move a documentation comment below a regular
  comment following it. This snippet of code is left as it is by the formatter:

  ```gleam
  /// This doc comment will be ignored!
  // a commented definition
  // fn wibble() {}

  /// Wibble's documentation.
  fn wibble() {
    todo
  }
  ```

  While previously all documentation comments would be merged together into one,
  ignoring the regular comment separating them:

  ```gleam
  // a commented definition
  // fn wibble() {}

  /// This doc comment will be ignored!
  /// Wibble's documentation.
  fn wibble() {
    todo
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Language Server

- The language server can now show completions for fields if a record access is
  being attempted.
  ([Ameen Radwan](https://github.com/Acepie))

- The language server will now insert a blank line before the first statement
  when inserting a new import and there are no other imports at the top of the
  module.
  ([Zhomart Mukhamejanov](https://github.com/Zhomart))

- The language server now suggests a code a action to rename variables, types
  and functions when they don't match the Gleam naming requirements:

  ```gleam
  let myNumber = 10
  ```

  Becomes:

  ```gleam
  let my_number = 10
  ```

  ([Surya Rose](https://github.com/gearsdatapacks))

- The language server can now suggest a code action to convert `let assert` into
  a case expression:

  ```gleam
  let assert Ok(value) = get_result()
  ```

  Becomes:

  ```gleam
  let value = case get_result() {
    Ok(value) -> value
    _ -> panic
  }
  ```

  ([Surya Rose](https://github.com/gearsdatapacks))

- The language server can now show signature help when writing functions.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now supports listing document symbols, such as functions
  and constants, for the current Gleam file.
  ([PgBiel](https://github.com/PgBiel))

- The language server can now suggest a code action to automatically use
  shorthand labels where possible:

  ```gleam
  case date {
    Day(day: day, month: month, year: year) -> todo
  }
  ```

  Becomes:

  ```gleam
  case date {
    Day(day:, month:, year:) -> todo
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server can now show completions for labels when writing a
  function call or record construction.
  ([Ameen Radwan](https://github.com/Acepie))

- The language server can now suggest a code action to fill in the labels of a
  function call:

  ```gleam
  pub type Date {
    Date(year: Int, month: Int, day: Int)
  }

  pub fn main() {
    Date()
  }
  ```

  Becomes:

  ```gleam
  pub type Date {
    Date(year: Int, month: Int, day: Int)
  }

  pub fn main() {
    Date(year: todo, month: todo, day: todo)
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Completions are now sorted by priority based on why the completion is in the
  list. This means that more specific completions like labels and local
  definitions will be shown before more broad completions like functions from a
  not yet imported module.
  ([Ameen Radwan](https://github.com/Acepie))

### Bug Fixes

- Functions, types and constructors named `module_info` are now escaped
  in generated Erlang code to avoid conflicts with the builtin
  `module_info/0` and `module_info/1` functions.
  ([Juraj Petráš](https://github.com/Hackder))

- Fixed formatting of comments at the start of a case branch.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where a private type could be leaked from an internal module.
  ([Ameen Radwan](https://github.com/Acepie))

- Fixed a bug where certain binops would not wrap their arguments properly
  thus generating invalid JavaScript.
  ([Ameen Radwan](https://github.com/Acepie))

- Fixed formatting of function definitions marked as `@internal`
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where importing a record constructor in an unqualified fashion and
  aliasing it and then using it in a case guard expression would generate
  invalid JavaScript.
  ([PgBiel](https://github.com/PgBiel))

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
