# Changelog

## Unreleased

### Bug fixes

- Fixed a bug where the "Generate function" code action would be incorrectly
  offered when calling a function unsupported by the current target, leading to
  invalid code if the code action was accepted.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Compiler

- Patterns aliasing a string prefix have been optimised to generate faster code
  on the Erlang target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

### Language server

- The "inline variable" code action can now trigger when used over the let
  keyword of a variable to inline.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

### Bug fixes

- Fixed a typo in the error message emitted when trying to run a module that
  does not have a main function.
  ([Louis Pilfold](https://github.com/lpil))
