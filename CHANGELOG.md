# Changelog

## Unreleased

### Build tool

- A helpful error message is now shown if the `manifest.toml` file has been
  edited to be invalid in some way.

  ```
  error: Corrupt manifest.toml

  The `manifest.toml` file is corrupt.

  Hint: Please fun `gleam update` to fix it.
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

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

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

### Language Server

- The code action to remove unused imports now removes the entire line is
  removed if it would otherwise be left blank.
  ([Milco Kats](https://github.com/katsmil))

- Hover for type annotations is now separate from the thing being annotated ([Ameen Radwan](https://github.com/Acepie))

- Go to definition now works for direct type annotations ([Ameen Radwan](https://github.com/Acepie))

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
