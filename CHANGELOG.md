# Changelog

## Unreleased

### Compiler

- Update error message that arises when calling `echo` on an atom that lacks a
  gleam representation to use `atom.create("__struct__")` instead of
  `atom.create_from_string("__struct__")`.
  ([Patrick Dewey](https://github.com/ptdewey))

- Patterns aliasing a string prefix have been optimised to generate faster code
  on the Erlang target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Analysis is now fault tolerant in the presence of errors in field definitions
  of custom type variants.
  ([Adi Salimgereyev](https://github.com/abs0luty))

- The compiler now emits a warning when a module contains no public definitions
  and prevents publishing packages with empty modules to Hex.
  ([Vitor Souza](https://github.com/vit0rr))

- The `@external` annotation is now supported for types with no constructors. It
  allows users to point an external type definition to a specific Erlang or
  TypeScript type. For example, the `dict.Dict` type from the standard library
  can now be written as the following:

  ```gleam
  @external(erlang, "erlang", "map")
  @external(javascript, "../dict.d.mts", "Dict")
  pub type Dict(key, value)
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- When matching the wrong number of subjects, the compiler now pinpoints the
  error location instead of marking the entire branch.

  ```
  case wibble {
    0, _ -> 1
    ^^^^ Expected 1 patterns, got 2
    0 |  -> 1
      ^ I was expecting a pattern after this
  }
  ```

  ([fruno](https://github.com/fruno-bulax/))

- Fixed two bugs that made gleam not update the manifest correctly, causing
  it to hit hex for version resolution on every operation and quickly reach
  request limits in large projects.
  ([fruno](https://github.com/fruno-bulax/))

- The performance of `==` and `!=` has been improved for single-variant custom
  types when compiling to JavaScript. This was done by generating comparison
  code specific to the custom type rather than using the generic equality check
  code.
  ([Nafi](https://github.com/re-masashi))

- The lowercase bool pattern error is no longer a syntax error, but instead a
  part of the analysis step. This allows the entire module to be analyzed, rather
  than stopping at the syntax error.
  ([mxtthias](https://github.com/mxtthias))

- Exhaustiveness checks for ints and floats now correctly handle unreachable
  cases in which the numbers contain underscores (i.e. `10` and `1_0`).
  Float exhaustiveness checks also now correctly identify unreachable cases
  containing scientific notation or trailing zeros (i.e. `100` and `1e2`).
  ([ptdewey](https://github.com/ptdewey))

- The compiler now emits a warning when a doc comment is not attached to a
  definition due to a regular comment in between. For example, in the following
  code:

  ```gleam
  /// This documentation is not attached
  // This is not a doc comment
  /// This is actual documentation
  pub fn wibble() {
    todo
  }
  ```

  Will now produce the following warning:

  ```txt
    warning: Detached doc comment
    ┌─ src/main.gleam:1:4
    │
  1 │ /// This documentation is not attached
    │    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ This is not attached to a definition

  This doc comment is followed by a regular comment so it is not attached to
  any definition.
  Hint: Move the comment above the doc comment
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- The [interference-based pruning](https://gleam.run/news/formalising-external-apis/#Improved-bit-array-exhaustiveness-checking)
  from 1.13 has been extended to int segments!
  Aside from the various performance improvements, this allows the compiler to
  mark more branches as unreachable.
  ```gleam
  case bits {
    <<"a">> -> 0
    <<97>> -> 1
    // ^- This branch is unreachable because it's equal to "a".

    <<0b1:1, _:1>> -> 2
    <<0b11:2>> -> 3
    // ^- This branch is unreachable because the branch before it already covers it.

    _ -> 99
  }
  ```
  ([fruno](https://github.com/fruno-bulax/))

### Build tool

- The help text displayed by `gleam dev --help`, `gleam test --help`, and
  `gleam run --help` has been improved: now each one states which function it's
  going to run.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The `--invert` and `--package` options of `gleam deps tree` are now mutually
  exclusive; if both options are given the command will fail. Previously,
  `--invert` would be silently ignored if given together with `--package`.
  ([Evan Silberman](https://github.com/silby))

- Update to latest Elixir API, so warning would not be shown when compiling
  Elixir file in a Gleam project.
  ([Andrey Kozhev](https://github.com/ankddev))

- The build tool now has a new `gleam deps outdated` command that shows outdated
  versions for dependencies. For example:

  ```sh
  $ gleam deps outdated
  Package  Current  Latest
  -------  -------  ------
  wibble   1.4.0    1.4.1
  wobble   1.0.1    2.3.0
  ```

  ([Vladislav Shakitskiy](https://github.com/vshakitskiy))

- The format used for `gleam deps list` and the notice of available major
  version upgrades has been improved.
  ([Louis Pilfold](https://github.com/lpil))

- `gleam new` now creates the project directory using the confirmed project
  name when a suggested rename is accepted.
  ([Adi Salimgereyev](https://github.com/abs0luty))

- The build tool now provides better error message when trying to build Git
  dependencies without Git installed. Previously, it would show this error:

  ```txt
  error: Shell command failure

  There was a problem when running the shell command `git`.

  The error from the shell command library was:

      Could not find the stdio stream
  ```

  Now it will show:

  ```txt
  error: Program not found

  The program `git` was not found. Is it installed?

  Documentation for installing Git can be viewed here:
  https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
  ```

  ([Andrey Kozhev](https://github.com/ankddev))

### Language server

- The "inline variable" code action can now trigger when used over the let
  keyword of a variable to inline.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The "add omitted labels" code action can now be used in function calls where
  some of the labels have been provided already.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Grouping of related diagnostics should now work across more editors.
  Warnings will display together with their hints and you no longer have
  "go to next diagnostic" twice in a row. Zedlings rejoice!
  ([fruno](https://github.com/fruno-bulax/))

- The "pattern match on variable" code action can now pick better names when
  used on tuples.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- When renaming, if the new name is invalid, the language server will produce an
  error message instead of silently doing nothing.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

### Bug fixes

- Fixed a bug where renaming a variable from an alternative pattern would not
  rename all its occurrences.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a typo in the error message emitted when trying to run a module that
  does not have a main function.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where the "Generate function" code action would be incorrectly
  offered when calling a function unsupported by the current target, leading to
  invalid code if the code action was accepted.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the formatter would not remove the right number of double
  negations from literal integers.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a typo for the "Invalid number of patterns" error.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a stack overflow when type checking some case expressions with
  thousands of branches.
  ([fruno](https://github.com/fruno-bulax/))

- The "add omitted label" code action no longer adds labels to arguments
  being piped in or the callbacks of `use`.
  ([fruno](https://github.com/fruno-bulax))

- Add a missing BitArray constructor return type in the prelude's TypeScript
  definitions.
  ([Richard Viney](https://github.com/richard-viney))

- Fixed a bug where the BEAM would be shut down abruptly once the program had
  successfully finished running.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where the "pattern match on variable" code action would generate
  invalid code when applied on a list's tail.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the "pattern match on variable" code action would generate
  invalid patterns by repeating a variable name already used in the same pattern.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where useless comparison warnings for floats compared literal
  strings, claiming for example that `1.0 == 1.` was always false.
  ([fruno](https://github.com/fruno-bulax/))

- Fixed a bug where pattern variables in case clause guards would incorrectly
  shadow outer scope variables in other branches when compiling to JavaScript.
  ([Elias Haider](https://github.com/EliasDerHai))

- Fix invalid TypeScript definition being generated for variant constructors
  with long names that take no arguments.
  ([Richard Viney](https://github.com/richard-viney))

- Fixed a bug where the formatter would remove the `@deprecated` attribute from
  constants.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fix invalid JavaScript codegen in cases where underscores follow a decimal.
  ([Patrick Dewey](https://github.com/ptdewey))

- Typos in the error message shown when trying to install a non-existent package
  have been fixed.
  ([Ioan Clarke](https://github.com/ioanclarke))
