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

### Build tool

- `gleam remove` now rejects invalid package names with an error explaining
  naming rules, instead of stating that the package is not a dependency.
  Trying to remove `gleam_otp@1` now suggests the matching dependency
  `gleam_otp`, if there is one.
  ([Tom Voet](https://github.com/tomvoet))

### Language server

- The language server will now offer "Discard unused variable" on each of
  alternative patterns.
  ([Andrey Kozhev](https://github.com/ankddev))

- The language server "Pattern match on variable" will now replace pattern with
  its expansion when triggered on discard in `use` assignment.
  ([Andrey Kozhev](https://github.com/ankddev))

- The language server "Pattern match on variable" code action has been renamed
  to "Pattern match on value" on discards in `use` assignments, so it's now
  consistent with discards in `let` assignments.
  ([Andrey Kozhev](https://github.com/ankddev))

- The language server will no longer show "Pattern match on value" code action
  on discards with multiple expansions in `use` assignments.
  ([Andrey Kozhev](https://github.com/ankddev))

- The language server will now show "Pattern match on value" code action for
  assignments inside patterns in `use` assignments.
  ([Andrey Kozhev](https://github.com/ankddev))

### Bug fixes

- Fixed a bug where the language server "Generate variant" code action would
  duplicate module name when triggered on qualified values.
  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where the language server would incorrectly show "Pattern match
  on argument" code action for patterns in `use` assignments.
  ([Andrey Kozhev](https://github.com/ankddev))

## v1.19.1 - 2026-10-07

### Bug fixes

- Fixed a bug where the `export javascript-prelude` and `export
  typescript-prelude` commands would not run.
  ([Louis Pilfold](https://github.com/lpil))
