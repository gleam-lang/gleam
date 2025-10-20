# Changelog

## Unreleased

### Compiler

- Patterns aliasing a string prefix have been optimised to generate faster code
  on the Erlang target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Type mismatch errors now emphasize the exact location of mismatches in complex
  types by highlighting the mismatched type parameters. This makes it easier to
  spot errors in types with multiple parameters or deep nesting.
  ([Adi Salimgereyev](https://github.com/abs0luty))

### Build tool

- The help text displayed by `gleam dev --help`, `gleam test --help`, and
  `gleam run --help` has been improved: now each one states which function it's
  going to run.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Language server

- The "inline variable" code action can now trigger when used over the let
  keyword of a variable to inline.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

### Bug fixes

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
