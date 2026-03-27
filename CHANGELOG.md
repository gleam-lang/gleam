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

- More readable error message when trying to revert an old release.
  ([Moritz Böhme](https://github.com/MoritzBoehme))

- The build tool now includes destination path in the error when it fails to
  link or copy file or directory.
  ([Andrey Kozhev](https://github.com/ankddev))

### Language server

- The language server now supports go-to-definition, find-references and rename
  for record fields. These work on the field declaration, on labelled arguments,
  on labelled patterns, on record updates and on `record.field` accesses, both
  within a module and across modules. For example:

  ```gleam
  pub type Person {
    Person(name: String, age: Int)
  }

  pub fn main() {
    let lucy = Person(name: "Lucy", age: 10)
    lucy.name
    //   ^ Go-to-definition jumps to the `name` field, and renaming it here
    //     renames the field everywhere it is used.
  }
  ```

  ([Alistair Smith](https://github.com/alii))

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

- The "remove unreachable patterns" code action can now be triggered on
  unreachable alternative patterns of a case expression.
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

- The language server now offers a code action to generate a missing type
  definition when an unknown type is referenced. For example, if `Wibble`
  is not defined:

  ```gleam
  pub fn run(data: Wibble(Int)) { todo }
  ```

  The code action will generate:

  ```gleam
  pub type Wibble(a)

  pub fn run(data: Wibble(Int)) { todo }
  ```

  ([Daniele Scaratti](https://github.com/lupodevelop))

- The language server now has "Discard unused variable" code action to discard
  unused variables in different places. For example,

  ```gleam
  pub type Wibble {
    Wibble(a: Int)
  }

  pub fn go(record: Wibble) -> Nil {
    let wibble = 0
    //  ^ Trigger code action here

    case record {
      Wibble(a:) -> Nil
      //     ^ Trigger code action here
    }

    case [0, 1, 2] {
      [_, ..] as wobble -> Nil
      //         ^ Trigger code action here
      [] -> Nil
    }

    Nil
  }
  ```

  Triggering the code action in all of these places would produce following code:

  ```gleam
  pub type Wibble {
    Wibble(a: Int)
  }

  pub fn go(record: Wibble) -> Nil {
    let _wibble = 0

    case record {
      Wibble(a: _) -> Nil
    }

    case [0, 1, 2] {
      [_, ..] -> Nil
      [] -> Nil
    }

    Nil
  }
  ```

  ([Andrey Kozhev](https://github.com/ankddev))

- The language server will now better rename types and values with import
  aliases by removing `as ...` part in case new name is same as original name of
  item. For example:

  ```gleam
  import wibble.{type Wibble as Wobble, Wibble as Wobble}

  pub fn go() -> Wobble {
    //           ^^^^^^ Rename to `Wibble`
    Wobble
  //^^^^^^ Rename to `Wibble`
  }
  ```

  Will now result in this code:

  ```gleam
  import wibble.{type Wibble, Wibble}

  pub fn go() -> Wibble {
    Wibble
  }
  ```

  ([Andrey Kozhev](https://github.com/ankddev))

- The language server now supports folding of comments and documentation
  comments. For example, this code:

  ```gleam
  //// Very useful module.
  ////
  //// It could be used to make interesting things

  /// Function to wibble.
  ///
  /// Not that it wobbles when wubble is true
  pub fn wibble() {
    // This todo here is temporary.
    // It will need to be removed at some moment.
    todo
  }
  ```

  can now be folded to:

  ```gleam
  //// Very useful module. ...

  /// Function to wibble. ...
  pub fn wibble() {
    // This todo here is temporary. ...
    todo
  }
  ```

  ([Andrey Kozhev](https://github.com/ankddev))

- The "Convert to function call" code action will now convert the currently
  hovered call and not only the final one.
  ([Andrey Kozhev](https://github.com/ankddev))

### Formatter

- Performance of the formatter has been improved.
  `gleam format` has been measured to be up to 13% faster on projects like
  `lustre`, with a 10% smaller peak memory footprint.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

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

- A `gleam@@compile.erl` is no longer left in the build output of
  `gleam compile-package`.
  ([Louis Pilfold](https://github.com/lpil))

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

- Fixed a bug where the JavaScript code generator could produce duplicate `let`
  declarations when a variable was reassigned after being shadowed inside a
  directly matching `case` branch.
  ([Eyup Can Akman](https://github.com/eyupcanakman))

- Fixed a bug where the language server would produce wrong code when triggering
  rename of types and values with import aliases.
  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where referencing qualified constructors in constant where a value
  of the same name exists in scope would cause invalid code to be generated.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug that would result in `gleam build` being slower than necessary
  when finding the Gleam files of a package.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
