# Changelog

## Unreleased

### Compiler

- The compiler now reports an error when integer and float binary operators are
  used incorrectly in case expression guards.
  ([Adi Salimgereyev](https://github.com/abs0luty))

- The compiler now supports string concatenation in clause guards:

  ```gleam
  case message {
    #(version, action) if version <> ":" <> action == "v1:delete" ->
      handle_delete()
    _ -> ignore()
  }
  ```

  ([Adi Salimgereyev](https://github.com/abs0luty))

### Build tool

- When adding a package that does not exist on Hex, the message is a bit
  friendlier.
  ([Ameen Radwan](https://github.com/Acepie))

- The `gleam.toml` format is now consistent. The two sausage-case fields
  (`dev-dependencies` and `tag-prefix`) have been replaced by snake_case
  versions. Files using the old names will continue to work.
  ([Louis Pilfold](https://github.com/lpil))

### Language server

- The language server now allows extracting the start of a pipeline into a
  variable.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

### Bug fixes

- Fixed a bug where some bit array patterns would erroneously be marked as
  unreachable.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now correctly tracks the minimum required version for constant
  record updates to be `>= 1.14.0`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now correctly tracks the minimum required version for
  expressions in `BitArray`s' `size` option to be `>= 1.12.0`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the formatter would not properly format some function calls
  if the last argument was followed by a trailing comment.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would generate invalid code on the JavaScript
  target when using a `case` expression as the right hand side of an equality
  check in an `assert`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server no longer recommends the deprecated `@target` attribute.
  ([Hari Mohan](https://github.com/seafoamteal))

- The compiler no longer crashes when trying to pattern match on a
  `UtfCodepoint`.
  ([Hari Mohan](https://github.com/seafoamteal))

- Fixed a bug that would result in not being able to rename an aliased pattern.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Added an error message when attempting to update packages that are not
  dependencies of the project, instead of failing silently.
  ([Etienne Boutet](https://github.com/EtienneBoutet),
  [Vladislav Shakitskiy](https://github.com/vshakitskiy))

- The build tool now doesn't perform code generation when exporting package
  interface.
  ([Andrey Kozhev](https://github.com/ankddev))

- The "Extract constant" code action now correctly places new constant when
  function has documentation. For example,

  ```gleam
  /// Wibble does some wobbling
  pub fn wibble() {
    let x = "wobble"
    //  ^ Trigger "Extract constant" here
    x
  }
  ```

  Previously, it would incorrectly place it below doc comment:

  ```gleam
  /// Wibble does some wobbling
  const x = "wobble"

  pub fn wibble() {
    x
  }
  ```

  Now it will correctly place constant above doc comment:

  ```gleam
  const x = "wobble"

  /// Wibble does some wobbling
  pub fn wibble() {
    x
  }
  ```

  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where renaming would not work properly if there was an error in
  target file.
  ([Surya Rose](https://github.com/GearsDatapacks))
