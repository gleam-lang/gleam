<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2020 The Gleam contributors
-->

# Changelog

## Unreleased

### Compiler

- The compiler now issues a friendlier error when attempting to pattern match
  on both the prefix and suffix of a string:

  ```
  error: Syntax error
    ┌─ /src/parse/error.gleam:2:23
    │
  2 │     "prefix" <> infix <> "suffix" -> infix
    │                       ^^^^^^^^^^^ This pattern is not allowed

  A string pattern can only match on a literal string prefix.

  Matching on a literal suffix is not possible, because `infix` would have an
  unknown size.
  ```

  ([Gavin Morrow](https://github.com/gavinmorrow))

- Improved the error message shown when using an invalid discard name for
  functions, constants, module names, and `as` patterns.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now generates singleton values for variants with no fields on the
  JavaScript target, allowing for faster comparison in most cases.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Build tool

- The build tool now generates Hexdocs URLs using the new format of
  `package.hexdocs.pm` rather than `hexdocs.pm/package`.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Language server

- The "pattern match on value" code action can now be used to pattern match on
  values returned by function calls. For example:

  ```gleam
  pub fn main() {
    load_user()
  // ^^ Triggering the code action over here
  }

  fn load_user() -> Result(User, Nil) { todo }
  ```

  Will produce the following code:

  ```gleam
  pub fn main() {
    case load_user() {
      Ok(value) -> todo
      Error(value) -> todo
    }
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now permits renaming type variables in functions, types,
  and constants. For example:

  ```gleam
  pub fn twice(value: a, f: fn(a) -> a) -> a {
    //                ^ Rename to "anything"
    f(f(value))
  }
  ```

  Produces:

  ```gleam
  pub fn twice(value: anything, f: fn(anything) -> anything) -> anything {
    f(f(value))
  }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- The language server now automatically updates imports when a Gleam module is
  renamed. For example:

  ```gleam
  import db_users

  pub fn main() -> db_users.User {
    db_users.new("username")
  }
  ```

  Renaming `db_users.gleam` to `database/user.gleam` would produce:

  ```gleam
  import database/user

  pub fn main() -> user.User {
    user.new("username")
  }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- Accepting an autocomplete inside the brackets of an import previously
  removed spaces following the previous import. For example:

  ```gleam
  import my_package/my_file.{MyFirstImport, MySecondImp|}
  //                                                   ^ cursor

  // Accepting the autocompletion produced:
  import my_package/my_file.{MyFirstImport,MySecondImport}
  ```

  That has been fixed to produce:

  ```gleam
  import my_package/my_file.{MyFirstImport, MySecondImport}

  ```

  ([Bruno Parga](https://github.com/brunoparga))

### Formatter

### Bug fixes

- When using the language server to extract a function from within an anonymous
  function, the return value of the extracted function is respected.

  For example,

  ```gleam
  fn wibble() {
    let wobble = fn() {
      let random_number = 4
      random_number * 42 // <- Extracting this line
    }
  }
  ```

  is turned into

  ```gleam
  fn wibble() {
    let wobble = fn() {
      let random_number = 4
      function(random_number)
    }
  }
  fn function(random_number: Int) -> Int {
    random_number * 42
  }
  ```

  ([Gavin Morrow](https://github.com/gavinmorrow))

- Work around an ambiguity of the language server protocol that resulted in
  editors like Zed inserting the wrong text when accepting type completions.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the post-publish message for pushing a git commit was
  formatted incorrectly.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where the compiler would generate invalid TypeScript type
  definitions for records with a field named `constructor`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would raise a warning for truncated int
  segments when compiling a function with a JavaScript external.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- When using the language server to extract a function from within the body of a
  use statement, only the selected statement(s) are extracted.

  For example,
  ```gleam
  pub fn wibble() {
      use wobble <- result.map(todo)
      echo wobble as "1" // <- Extracting this line
      echo wobble as "2"
  }
  ```
  is turned into
  ```gleam
  pub fn wibble() {
      use wobble <- result.map(todo)
      function(wobble)
      echo wobble as "2"
  }
  fn function(wobble: a) -> Nil {
    echo wobble as "1"
    Nil
  }
  ```
  ([Gavin Morrow](https://github.com/gavinmorrow))
