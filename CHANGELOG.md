<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2020 The Gleam contributors
-->

# Changelog

## Unreleased

### Compiler

- When compiling to JavaScript any case clauses found to be unreachable will no
  longer generate any code.
  ([Jack Programs](https://github.com/jackprogramsjp))

- The compiler now has a specific error message for when trying to use
  procedural operators that do not exist in Gleam, such as `+=` and `++`.
  ([0xda157](https://github.com/0xda157))

- The error message for calling a function in a guard expression or constant
  expression now points to the entire function call.
  ([mmustafasenoglu](https://github.com/mmustafasenoglu))

- Pattern matching on the JavaScript target now generates flatter code, with
  nested `if` statements collapsed into a single condition and fewer
  intermediate variables. This makes little difference to the size of a bundled
  application, but the resulting code has fewer branches for JavaScript engines
  to optimise.
  ([John Downey](https://github.com/jtdowney))

- When producing TypeScript annotations, the compiler now produces overloads to
  narrow the type of variants of a custom type. For example, given the following
  type:

  ```gleam
  pub type Box(value) {
    Full(value)
    Empty
  }
  ```

  Will now produce these two additional overloads:

  ```ts
  export function Box$isFull<I>(value: Box$<I>): value is Full<I>;
  export function Box$isEmpty<I>(value: Box$<I>): value is Empty;
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

- The build tool now stores its build cache in a more compact binary format,
  making the cache files noticeably smaller.
  ([Andrey Kozhev](httos://github.com/ankddev))

- Make links to Tangled repositories use their new domain & URL format.
  ([Naomi Roberts](https://github.com/naomieow))

### Language server

- The "Generate dynamic decoder" code action is now only offered when the
  `gleam_stdlib` package is available.
  ([0xda157](https://github.com/0xda157))

### Formatter

### Bug fixes

- Fixed a bug where on the JavaScript target a case clause whose guard's top
  level operator was `||` could run for a subject its pattern did not match.
  ([John Downey](https://github.com/jtdowney))

- Fixed a bug where `gleam export erlang-shipment` would add Gleam contributor
  copyright metadata to the generated POSIX entrypoint.
  ([John Downey](https://github.com/jtdowney))

- Fixed a bug where the `echo` on the JavaScript target would incorrectly print
  numbers like `1.0e300`.
  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where the `echo` on the JavaScript target would incorrectly print
  `NaN` and `Infinity`.
  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where the language server would incorrectly rename values and
  types with a same-named import alias.
  ([Andrey Kozhev](https://github.com/ankddev))

## v1.18.1 - 2026-08-01

- Fixed a bug where the Erlang code generator would generate wrong code when
  referencing constants that are aliases to other constants in bit array
  segments, string concatenation and clause guards.
  ([Andrey Kozhev](https://github.com/ankddev))
