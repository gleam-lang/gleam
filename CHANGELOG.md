# Changelog

## Unreleased

### Compiler

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

- The lowercase bool pattern error is no longer a syntax error, but instead a
  part of the analysis step. This allows the entire module to be analyzed, rather
  than stopping at the syntax error.
  ([mxtthias](https://github.com/mxtthias))

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

### Language server

- The "inline variable" code action can now trigger when used over the let
  keyword of a variable to inline.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The "pattern match on variable" code action can now pick better names when
  used on tuples.
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
