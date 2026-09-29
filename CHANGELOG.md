<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2020 The Gleam contributors
-->

# Changelog

## Unreleased

### Build tool

- If the source repo when building HexDocs is unset or an unknown forge, fall 
  back to using Hex links for the source.
  ([Alex Hinde](https://github.com/apex-hinde))

## 1.19.0-rc2 - 2026-09-26

## Compiler

- The `--src-only` flag has been renamed to `--no-dev`.
  ([Louis Pilfold](https://github.com/lpil))

- The error message for failing to load data about already compiled modules has
  been improved.
  ([Louis Pilfold](https://github.com/lpil))

## Bug fixes

- Fixed a bug with `gleam compile-package` where `dev_dependencies` would not
  be included in the `.app` file regardless of whether `--no-dev` was provided.
  ([Rodrigo Álvarez](https://github.com/Papipo))

- Fixed a bug where `gleam compile-package` would not use Erlang abstract forms
  for compilation to BEAM.
  ([Louis Pilfold](https://github.com/lpil))

## 1.19.0-rc1 - 2026-09-22

### Compiler

- The compiler will now show "Unused variable" warning for each of alternative
  patterns.
  ([Andrey Kozhev](https://github.com/ankddev))

### Build tool

- `gleam remove` now rejects invalid package names with an error explaining
  naming rules, instead of stating that the package is not a dependency.
  Trying to remove `gleam_otp@1` now suggests the matching dependency
  `gleam_otp`, if there is one.
  ([Tom Voet](https://github.com/tomvoet))

- When Hex dependencies change version the build tool now prints a link to the
  diff on Hex, making it easier to review and audit dependency updates.
  ([Manas Ganesh Dasari](https://github.com/ManasDasri))

### Language server

- The language server will now offer "Discard unused variable" on each of
  alternative patterns.
  ([Andrey Kozhev](https://github.com/ankddev))

- The language server now allows to find references of `echo`. For example,

  ```gleam
  pub fn wibble(woo) {
    echo woo
  //^^^^
    echo wobble(woo)
  //^^^^
  }

  pub fn wobble(woo) {
    echo woo
  //^^^^
  }
  ```

  When triggering on any of denoted places, it will show all usages.
  ([Andrey Kozhev](https://github.com/ankddev))

- The language server now allows to highlight all `echo` in file. For example,

  ```gleam
  pub fn wibble(woo) {
    echo woo
  //^^^^
    echo wobble(woo)
  //^^^^
  }

  pub fn wobble(woo) {
    echo woo
  //^^^^
  }
  ```

  If this feature is enabled in your editor, hovering any of denoted places will
  highlight all of them.
  ([Andrey Kozhev](https://github.com/ankddev))

- The language server now allows to find references of `todo`. For example,

  ```gleam
  pub const wubble = todo
  //                 ^^^^

  pub fn wibble() {
    todo as "unimplemented yet"
  //^^^^^^^^^^^^^^^^^^^^^^^^^^^
  }

  pub fn empty() {}
  //^^^^^^^^^^^^ Empty functions are shown too!

  pub fn block() {
    {}
  //^^ And empty blocks too!
  }
  ```

  When triggering on any of denoted places, it will show all usages.
  ([Andrey Kozhev](https://github.com/ankddev))

- The language server now allows to highlight all `todo` in file. For example,

  ```gleam
  pub const wubble = todo
  //                 ^^^^

  pub fn wibble() {
    todo as "unimplemented yet"
  //^^^^^^^^^^^^^^^^^^^^^^^^^^^
  }

  pub fn empty() {}
  //^^^^^^^^^^^^ Empty functions are highlighted too!

  pub fn block() {
    {}
  //^^ And empty blocks too!
  }
  ```

  If this feature is enabled in your editor, hovering any of denoted places will
  highlight all of them.
  ([Andrey Kozhev](https://github.com/ankddev))

- The language server now allows to find references of `panic`. For example,

  ```gleam
  pub fn wibble() {
    panic
  //^^^^^
  }

  pub fn wobble() {
    panic as "unimplemented"
  //^^^^^^^^^^^^^^^^^^^^^^^^
  }
  ```

  When triggering on any of denoted places, it will show all usages.
  ([Andrey Kozhev](https://github.com/ankddev))

- The language server now allows to highlight all `panic` in file. For example,

  ```gleam
  pub fn wibble() {
    panic
  //^^^^^
  }

  pub fn wobble() {
    panic as "unimplemented"
  //^^^^^^^^^^^^^^^^^^^^^^^^
  }
  ```

  If this feature is enabled in your editor, hovering any of denoted places will
  highlight all of them.
  ([Andrey Kozhev](https://github.com/ankddev))

### Bug fixes

- Fixed a bug where the language server "Generate variant" code action would
  duplicate module name when triggered on qualified values.
  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where bad error message would be shown when trying to publish
  package with no README on Windows.
  ([Andrey Kozhev](https://github.com/ankddev))

## v1.19.1 - 2026-10-07

### Bug fixes

- Fixed a bug where the `export javascript-prelude` and `export
  typescript-prelude` commands would not run.
  ([Louis Pilfold](https://github.com/lpil))
