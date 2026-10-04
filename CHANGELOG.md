<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2020 The Gleam contributors
-->

# Changelog

## Unreleased

### Compiler

- The compiler will now show "Unused variable" warning for each of alternative
  patterns.
  ([Andrey Kozhev](https://github.com/ankddev))

- The compiler will now suggest types from imported modules with same name as
  unknown type.
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

- The language server will now offer a code action to use an item from imported
  module with same name as unknown type or value. For example,

  ```gleam
  import gleam/io
  import gleam/option

  pub fn main() -> Option {
  //               ^^^^^^ Trigger "Use `option.Option`"
    println("Hello!")
  //^^^^^^^ Trigger "Use `io.println`"
  }
  ```

  Triggering denoted code actions would result in this code:

  ```gleam
  import gleam/io
  import gleam/option

  pub fn main() -> option.Option {
    io.println("Hello!")
  }
  ```

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
