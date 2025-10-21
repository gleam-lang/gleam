# Changelog

## Unreleased

### Compiler

- Patterns aliasing a string prefix have been optimised to generate faster code
  on the Erlang target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Analysis it now fault tolerant in the presence of errors in field definitions
  of custom type variants.
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

- The "pattern match on variable" code action can now pick better names when
  used on tuples.
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
