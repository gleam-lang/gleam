# Changelog

## Unreleased

### Compiler

### Build tool

- When adding a package that does not exist on Hex, the message is a bit
  friendlier.
  ([Ameen Radwan](https://github.com/Acepie))

### Language server

- The language server now allows extracting the start of a pipeline into a
  variable.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

### Bug fixes

- The compiler now correctly tracks the minimum required version for constant
  record updates to be `>= 1.14.0`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now correctly tracks the minimum required version for
  expressions in `BitArray`s' `size` option to be `>= 1.12.0`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the formatter would not properly format some function calls
  if the last argument was followed by a trailing comment.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server no longer recommends the deprecated `@target` attribute.
  ([Hari Mohan](https://github.com/seafoamteal))

- The compiler no longer crashes when trying to pattern match on a
  `UtfCodepoint`.
  ([Hari Mohan](https://github.com/seafoamteal))

- Fixed a bug that would result in not being able to rename an aliased pattern.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
- Fixed two bugs that made gleam not update the manifest correctly, causing
  it to hit hex for version resolution on every operation and quickly reach
  request limits in large projects.
  ([fruno](https://github.com/fruno-bulax/))

- Fixed a bug where renaming a variable from an alternative pattern would not
  rename all its occurrences.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now reports an error for literal floats that are outside the
  floating point representable range on both targets. Previously it would only
  do that when compiling on the Erlang target.
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

- Fixed a bug that caused the compiler to incorrectly optimise away runtime
  size checks in bit array patterns on the javascript target if they used
  calculations in the size of a segment (`_:size(wibble - wobble)`).
  ([fruno](https://github.com/fruno-bulax/))

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
  invalid patterns by repeating a variable name already used in the same
  pattern.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the "generate function" code action would pop up for
  variants.
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

- Fixed a bug where invalid code would be generated on the JavaScript target in
  cases where an underscore followed the decimal point in a float literal.
  ([Patrick Dewey](https://github.com/ptdewey))

- Typos in the error message shown when trying to install a non-existent package
  have been fixed.
  ([Ioan Clarke](https://github.com/ioanclarke))

- Fixed a bug where the compiler would generate invalid Erlang and TypeScript
  code for unused opaque types referencing private types.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the type checker would allow invalid programs when a large
  group of functions were all mutually recursive.
  ([Surya Rose](https://github.com/GearsDatapacks))

- The compiler now provides a clearer error message when a function's return type
  is mistakenly declared using `:` instead of `->`.
  ([Gurvir Singh](https://github.com/baraich))

- Fixed a bug where the data generated for searching documentation was in the
  wrong format, preventing it from being used by Hexdocs search.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the "collapse nested case" code action would produce invalid
  code on a list tail pattern.
  ([Matias Carlander](https://github.com/matiascr))

- Fixed two bugs that made gleam not update the manifest correctly, causing
  it to hit hex for version resolution on every operation and quickly reach
  request limits in large projects.
  ([fruno](https://github.com/fruno-bulax/))
- Added an error message when attempting to update packages that are not dependencies
  of the project, instead of failing silently.
  ([Etienne Boutet](https://github.com/EtienneBoutet)) and ([Vladislav Shakitskiy](https://github.com/vshakitskiy))