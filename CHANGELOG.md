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

- The use of pipes to turn the Gleam code `a |> b(c)` into `b(c)(a)` has been
  deprecated.
  ([Surya Rose](https://github.com/GearsDatapacks))

- The compiler now gives a better error message when an `@external`
  attribute is incomplete. For example:

  ```gleam
  @external
  pub fn wibble()
  ```

  now points to the attribute itself and explains that it is incomplete.
  ([Asish Kumar](https://github.com/officialasishkumar))

### Build tool

- The build tool now generates Hexdocs URLs using the new format of
  `package.hexdocs.pm` rather than `hexdocs.pm/package`.
  ([Surya Rose](https://github.com/GearsDatapacks))

- More readable error message when trying to revert an old release.
  ([Moritz Böhme](https://github.com/MoritzBoehme))

- The build tool now includes destination path in the error when it fails to
  link or copy file or directory.
  ([Andrey Kozhev](https://github.com/ankddev))

- Git dependencies now support an optional `path` field to specify a
  subdirectory within the repository. This is useful for monorepos that
  contain multiple Gleam packages. For example:

  ```toml
  [dependencies]
  my_package = {
    git = "https://github.com/example/monorepo",
    ref = "main",
    path = "packages/my_package",
  }
  ```

  ([John Downey](https://github.com/jtdowney))

- The `gleam hex owner transfer` command now uses the flag `--user` instead of
  the flag `--to`. The `gleam hex owner add` command now takes the package name
  via the flag `--package`.
  ([Louis Pilfold](https://github.com/lpil))

- The error message when failing to decrypt the local Hex API key is now more
  informative and helpful.
  ([Moritz Böhme](https://github.com/MoritzBoehme))

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

- When using the "Extract function" code action on statements whose values are
  unused, the extracted function will return the last statement. For example:

  ```gleam
  fn main() {
  //↓ start selection here
    echo "line 2"
    echo "line 3"
  //            ↑ end selection here
    echo "line 4"
  }
  ```

  will be turned into

  ```gleam
  fn main() {
    function()
    echo "line 4"
  }

  fn function() -> String {
    echo "line 2"
    echo "line 3"
  }
  ```

  ([Gavin Morrow](https://github.com/gavinmorrow))

- The language server now offers a code action to fix the new deprecated pipeline
  syntax.
  ([Surya Rose](https://github.com/GearsDatapacks))

- The language server can now find references for and rename items when
  triggered from an import statement:

  ```gleam
  import wibble.{type Wibble}
  //                  ^^^^^^ Trigger find references or rename here

  pub fn main() {
    let _ = Wibble
  }
  ```

  ([Gavin Morrow](https://github.com/gavinmorrow))

- The language server now has "Convert to documentation comment" and
  "Convert to regular comment" code actions. For example:

  ```gleam
  // Module description.
  // Code action available here.

  // Comment before function.
  // Another code action here.
  pub fn wibble() {
    // No code action here.
    todo
  }

  /// Doc comment.
  /// Another code action here.
  pub fn wobble () {
    todo
  }
  ```

  Triggering the code actions in all of these places will result in:

  ```gleam
  //// Module description.
  //// Code action available here.

  /// Comment before function.
  /// Another code action here.
  pub fn wibble() {
    // No code action here.
    todo
  }

  // Doc comment.
  // Another code action here.
  pub fn wobble () {
    todo
  }
  ```

  ([Daniel Venable](https://github.com/DanielVenable))

### Formatter

- Performance of the formatter has been improved.
  `gleam format` has been measured to be up to 13% faster on projects like
  `lustre`, with a 10% smaller peak memory footprint.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Formatter now removes import aliases if the aliased name is the
  same as the original name. For example,

  ```gleam
  import wibble.{Wibble as Wibble}
  ```

  becomes

  ```gleam
  import wibble.{Wibble}
  ```

  ([Daniel Venable](https://github.com/DanielVenable))

### Bug fixes

- Fixed a bug where the generated Erlang `.app` file's `modules` list would only
  contain the modules recompiled by the latest build, becoming empty on a warm
  rebuild where nothing changed.
  ([Charlie Tonneslan](https://github.com/c-tonneslan))

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

- Fixed a bug where the compiler would generate invalid code for `let assert`
  expression with bit array patterns.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would evaluate the numerator and denominator
  of a division in the wrong order.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would generate Erlang code that raises further
  warnings for unused values.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would raise a warning for truncated int
  segments when compiling a function with a JavaScript external.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would produce a confusing error message when
  writing a constructor with a lowercase name.
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
  fn function(wobble: a) -> a {
    echo wobble as "1"
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

- The formatter now properly formats binary operations in bit array size
  segments.
  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where after removing dependencies with `gleam remove` if a
  removed dependency is still used the build would succeed, resulting in
  runtime crash due to missing files.
  ([Andrey Kozhev](https://github.com/ankddev))

- The build tool will no longer panic when unable to lock the build directory.
  ([Louis Pilfold](https://github.com/lpil))

- The formatter now properly indents multiline trailing comments inside of
  multiline lists and tuples.
  ([0xda157](https://github.com/0xda157))

- Fixed a bug where the compiler would panic on the first HTTPS request on
  Android.
  ([John Downey](https://github.com/jtdowney))

- Fixed a bug where the language server would not show autocomplete for record
  fields of internal types within the same package.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where warnings and errors in the arguments of a call to a
  function literal would be reported multiple times.
  ([John Downey](https://github.com/jtdowney))

- Fixed a bug where pattern matching on overlapping string prefixes with guards
  could generate incorrect JavaScript.
  ([John Downey](https://github.com/jtdowney))

- Fixed a bug where running `gleam docs build` on non-Erlang target projects
  with a warm cache would not produce the module pages.
  ([Matt Champagne](https://github.com/han-tyumi))

- Fixed a bug where an incorrect `package-interface.json` would be generated for
  certain type aliases.
  ([Surya Rose](https://github.com/GearsDatapacks))
