# Changelog

## Unreleased

### Build tool

- A helpful error message is now shown if the `manifest.toml` file has been
  edited to be invalid in some way.

  ```
  error: Corrupt manifest.toml

  The `manifest.toml` file is corrupt.

  Hint: Please run `gleam update` to fix it.
  ```

  ([zahash](https://github.com/zahash))

- The error message shown when unable to find package versions that satisfy all
  the version constraints specified for a project's dependencies has been
  greatly improved.

  ```
  error: Dependency resolution failed

  An error occurred while determining what dependency packages and
  versions should be downloaded.
  The error from the version resolver library was:

  Unable to find compatible versions for the version constraints in your
  gleam.toml. The conflicting packages are:

  - hellogleam
  - lustre_dev_tools
  - glint
  ```

  ([zahash](https://github.com/zahash))

- A link to the package on Hex is no longer auto-added to the HTML documentation
  when building them locally. It is still added when publishing to Hex.
  ([Pi-Cla](https://github.com/Pi-Cla))

- An error is now emitted when compiling to Erlang and there is a Gleam module
  that would overwrite a built-in Erlang/OTP module, causing cryptic errors and
  crashes.
  ([Louis Pilfold](https://github.com/lpil))

  ```
  error: Erlang module name collision

  The module `src/code.gleam` compiles to an Erlang module named `code`.

  By default Erlang includes a module with the same name so if we were to
  compile and load your module it would overwrite the Erlang one, potentially
  causing confusing errors and crashes.

  Hint: Rename this module and try again.
  ```

### Compiler

- The compiler will now raise a warning for `let assert` assignments where the
  assertion is redundant.

  ```
  warning: Redundant assertion
    ┌─ /home/lucy/src/app/src/app.gleam:4:7
    │
  4 │   let assert x = get_name()
    │       ^^^^^^ You can remove this

  This assertion is redundant since the pattern covers all possibilities.
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Empty case expressions are no longer parse errors and will instead be
  exhaustiveness errors. ([Race Williams](https://github.com/raquentin))

- Initial support for type analysis returning multiple errors. ([Ameen Radwan](https://github.com/Acepie))

- Improve error message if importing type using the value import syntax or vice versa.

  ```
  error: Unknown module field
    ┌─ /src/one/two.gleam:1:19
    │
  1 │ import gleam/one.{One}
    │                   ^^^ Did you mean `type One`?

  `One` is only a type, it cannot be imported as a value.
  ```

  ```
  error: Unknown module type
    ┌─ /src/one/two.gleam:1:19
    │
  1 │ import gleam/two.{type Two}
    │                   ^^^^^^^^ Did you mean `Two`?

  `Two` is only a value, it cannot be imported as a type.
  ```

  ([Pi-Cla](https://github.com/Pi-Cla/))

- The compiler will now raise a warning when you try to use `todo` or `panic` as
  if they were functions: this could previously lead to a confusing behaviour
  since one might expect the arguments to be printed in the error message.
  The error message now suggests the correct way to add an error message to
  `todo` and `panic`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

  ```
  warning: Todo used as a function
    ┌─ /src/warning/wrn.gleam:2:16
    │
  2 │           todo(1)
    │                ^

  `todo` is not a function and will crash before it can do anything with
  this argument.

  Hint: if you want to display an error message you should write
  `todo as "my error message"`
  See: https://tour.gleam.run/advanced-features/todo/
  ```

- Improve error message when something that is not a function appears on the
  right hand side of `<-` in a `use` expression.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

  ```txt
  error: Type mismatch
    ┌─ /src/one/two.gleam:2:8
    │
  2 │ use <- 123
    │        ^^^

  In a use expression, there should be a function on the right hand side of
  `<-`, but this value has type:

      Int

  See: https://tour.gleam.run/advanced-features/use/
  ```

- Improve error message when a function with the wrong number of arguments
  appears on the right hand side of `<-` in a `use` expression.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

  ```txt
  error: Incorrect arity
    ┌─ /src/one/two.gleam:3:8
    │
  3 │ use <- func
    │        ^^^^ Expected no arguments, got 1

  The function on the right of `<-` here takes no arguments.
  But it has to take at least one argument, a callback function.

  See: https://tour.gleam.run/advanced-features/use/
  ```

  ```txt
  error: Incorrect arity
    ┌─ /src/one/two.gleam:3:8
    │
  3 │ use <- f(1, 2)
    │        ^^^^^^^ Expected 2 arguments, got 3

  The function on the right of `<-` here takes 2 arguments.
  All the arguments have already been supplied, so it cannot take the the
  `use` callback function as a final argument.

  See: https://tour.gleam.run/advanced-features/use/
  ```

- Improve error message when a the callback function of a `use` expression
  returns a value with the wrong type.
  Now the error will point precisely to the last statement and not complain
  about the whole block saying it has the wrong function type.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler will now raise a warning when pattern matching on a literal value
  like a list, a tuple, integers, strings etc.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

  ```
  warning: Redundant list
    ┌─ /src/warning/wrn.gleam:2:14
    │
  2 │         case [1, 2] {
    │              ^^^^^^ You can remove this list wrapper

  Instead of building a list and matching on it, you can match on its
  contents directly.
  A case expression can take multiple subjects separated by commas like this:

      case one_subject, another_subject {
        _, _ -> todo
      }

  See: https://tour.gleam.run/flow-control/multiple-subjects/
  ```

  ```
  warning: Match on a literal value
    ┌─ /src/warning/wrn.gleam:4:8
    │
  4 │   case 1 {
    │        ^ There's no need to pattern match on this value

  Matching on a literal value is redundant since you can already tell which
  branch is going to match with this value.

- The compiler will now continue module analysis when there are errors in top level constant definitions. This means that multiple constant errors can be displayed at once if there are not other errors. ([Ameen Radwan](https://github.com/Acepie))

  ```
  error: Unknown type
    ┌─ /src/test_gleam.gleam:5:18
    │
  5 │ const my_string: MyInvalidType = "str"
    │                  ^^^^^^^^^^^^^

  The type `MyInvalidType` is not defined or imported in this module.

  error: Unknown variable
    ┌─ /src/test_gleam.gleam:7:35
    │
  7 │ const my_tuple: String = #(Ok(1), MyInvalidType, 3)
    │                                   ^^^^^^^^^^^^^

  The name `MyInvalidType` is not in scope here.
  ```

### Formatter

- Redundant alias names for imported modules are now removed.

  ```gleam
  import gleam/result as result
  ```

  is formatted to

  ```gleam
  import gleam/result
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Comments are no longer moved out of constant lists, constant tuples and empty
  tuples. You can now write this:

  ```gleam
  const values = [
    // This is a comment!
    1, 2, 3
    // Another comment...
    11,
    // And a final one.
  ]
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Comments at the end of an anonymous function are no longer moved out of it.
  You can now write this:

  ```gleam
  fn() {
    todo
    // A comment here!
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Pipes can now be placed on a single line if they are short enough:

  ```gleam
  [1, 2, 3] |> list.map(int.to_string) |> string.join(with: "\n")
  ```

  In addition you can also force the formatter to break a pipe on multiple lines
  by manually breaking it. This:

  ```gleam
  [1, 2, 3]
  // By putting a newline here I'm telling the formatter to split the pipeline
  |> list.map(int.to_string) |> string.join(with: "\n")
  ```

  Will turn into this:

  ```gleam
  [1, 2, 3]
  |> list.map(int.to_string)
  |> string.join(with: "\n")
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Comments appearing after arguments are not longer moved to different place.
  You can now write all of those:

  ```gleam
  type Record {
    Record(
      field: String,
      // comment_line_1: String,
      // comment_line_2: String,
    )
  }
  ```

  ```gleam
  pub fn main() {
    fn(
      a,
      // A comment 2
    ) {
      1
    }
  }
  ```

  ```gleam
  fn main() {
    let triple = Triple(1, 2, 3)
    let Triple(
      a,
      ..,
      // comment
    ) = triple
    a
  }
  ```

  ```gleam
  type Record {
    Record(
      // comment_line_1: String,
      // comment_line_2: String,
    )
  }
  ```

  ([Mateusz Ledwoń](https://github.com/Axot017))

### Language Server

- The code action to remove unused imports now removes the entire line is
  removed if it would otherwise be left blank.
  ([Milco Kats](https://github.com/katsmil))

- Hover for type annotations is now separate from the thing being annotated. ([Ameen Radwan](https://github.com/Acepie))

- Go to definition now works for direct type annotations. ([Ameen Radwan](https://github.com/Acepie))

- Go to definition now works for import statements. ([Ameen Radwan](https://github.com/Acepie))

- Hover now works for unqualified imports. ([Ameen Radwan](https://github.com/Acepie))

### Bug Fixes

- Fixed [RUSTSEC-2021-0145](https://rustsec.org/advisories/RUSTSEC-2021-0145) by
  using Rust's `std::io::IsTerminal` instead of the `atty` library.
  ([Pi-Cla](https://github.com/Pi-Cla))

- Fixed the generated `mod` property in the Erlang application file when using the
  `application_start_module` property in `gleam.toml`.
  ([Alex Manning](https://github.com/rawhat))

- Fixed some reserved keywords would resulting in confusing error messages.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed variables in constant expressions not being escaped correctly when
  exporting to JavaScript. ([PgBiel](https://github.com/PgBiel))

- Fixed a typo when attempting to publish a package with non-Hex dependencies
  ([inoas](https://github.com/inoas))

- Fixed import completions not appearing in some editors due to range being
  longer than line. ([Ameen Radwan](https://github.com/Acepie))

- Fixed a bug where TypeScript definitions files would use `null` instead of
  `undefined`.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where unreachable infinite cases would not be detected when
  after a discard or variable pattern.
  ([Ameen Radwan](https://github.com/Acepie)) and ([Pi-Cla](https://github.com/Pi-Cla))

- Fixed a bug where module imports in guard clauses would not be generated
  correctly for js target. ([Ameen Radwan](https://github.com/Acepie))

- Fixed a bug where formatting constant lists of tuples would be force the tuples
  to be broken across multiple lines, even when they could fit on a single line.
  ([Isaac Harris-Holt](https://github.com/isaacharrisholt))

- Fixed a bug where floating points in scientific notation with no trailing
  zeros would generate invalid Erlang code.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where having utf8 symbols in `gleam.toml`'s description value
  would result in an HTTP 500 error when running `gleam publish`.
  ([inoas](https://github.com/inoas))

- Unicode `\u{}` syntax in bit_array string segments now produce valid Erlang
  unicode characters ([Pi-Cla](https://github.com/Pi-Cla))

- Fixed a bug where using a constant defined in another module that referenced
  a private function could generate invalid code on the Erlang target.
  ([Shayan Javani](https://github.com/massivefermion))
