<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2020 The Gleam contributors
-->

# Changelog

## Unreleased

### Compiler

- When compiling to Erlang, Gleam now compiles to the Erlang Abstract Format
  instead of textual Erlang.
  This format supports mapping every generated Erlang line back to its
  corresponding line in the original Gleam source file.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- When compiling to Erlang the Gleam compiler is roughly 8% faster.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now emits a warning when trying to use the pattern of a spread
  and a variable when pattern matching inside a 'case' clause.
  This syntax may be removed in a future (major) release.

  Considering the following Gleam code:

  ```gleam
  pub fn main() {
    let letters = ["b", "c"]
    case letters {
      [] -> []
      [..x] -> x
    }
  }
  ```

  The compiler will emit the following warning:

  ```txt
  warning: Deprecated list pattern matching syntax
    ┌─ /main.gleam:6:9
    │
  6 │         [..x] -> x
    │         ^^^^^ This can be replaced with the variable itself
  ```

  ([Khalid Belkassmi E.H.](https://github.com/khalidbelk))

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

- The compiler is now fault tolerant when it runs into unsupported binary
  operators used in constants. Instead than stopping to analyse the whole module
  the compiler now produces an error message explaining the operator is not
  supported. For example:

  ```gleam
  pub const wibble = 1.1 *. 11.0
  ```

  Results in the following error:

  ```
  error: Unsupported operator in constant expression
    ┌─ /Users/giacomocavalieri/Desktop/prova/src/prova.gleam:1:24
    │
  1 │ pub const wibble = 1.1 *. 11.0
    │                        ^^

  This operator is currently not supported in constants.
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- When generating Erlang type definitions, types called `Record` will be renamed
  to `record_` to not overwrite Erlang's built-in `record` type introduced in
  OTP29.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now provides a specific error message directing the user to the
  correct syntax when trying to update a record after providing fields.
  ([0xda157](https://github.com/0xda157))

- The compiler now provides a more specific error message when encountering a
  merge conflict indicator
  ([0xda157](https://github.com/0xda157))

### Build tool

- The build tool now stores its build cache in a more compact binary format,
  making the cache files noticeably smaller.
  ([Andrey Kozhev](https://github.com/ankddev))

- The `ext` protocol is now explicitly disabled when when fetching git
  dependencies. This protocol is disabled by default since 2015, so this
  has no impact unless the user is using a very old version of git or has
  enabled this protocol in their configuration.
  ([Amr Kadry](https://github.com/Amrkadry) and
  ([Louis Pilfold](https://github.com/lpil))

- Make links to Tangled repositories use their new domain & URL format.
  ([Naomi Roberts](https://github.com/naomieow))

### Language server

- The "Generate dynamic decoder" code action is now only offered when the
  `gleam_stdlib` package is available.
  ([0xda157](https://github.com/0xda157))

- Renaming a prelude item to its original name no longer unnecessarily imports
  it.
  ([0xda157](https://github.com/0xda157))

- The "add missing patterns" code action now has higher precedence than the code
  action to discard an unused result.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The "pattern match on argument" code action now has higher precedence over the
  "discard unused argument" code action.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server will now surround value completions right after dot in
  imports with braces. For example, consider following code:

  ```gleam
  import wibble.|
  ```

  where cursor is denoted with `|`. Completing value `Wibble` will result in
  following code:

  ```gleam
  import wibble.{Wibble}
  ```

  ([Andrey Kozhev](https://github.com/ankddev))

- The language server now supports triggering "Convert int to different base"
  code action in constants, patterns and bit array "size" options.
  ([Andrey Kozhev](https://github.com/ankddev))

### Formatter

### Compiler Wasm API

- The WebAssembly build of the compiler now exposes a `format_source` function
  for formatting Gleam source code.
  ([John Downey](https://github.com/jtdowney))

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

- Fixed a bug where the language server would crash if the client disconnected
  unexpectedly.
  ([Senthilnathan](https://github.com/ssenthilnathan3))

- Fixed a bug where comments after the last item in a tuple, or after the last
  argument in a function call wouldn't be formatted properly.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where file permissions were not set in Hex tarballs.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where echo would print BitArrays like `<<1, 2, 3>>` as strings on
  the Erlang target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the "Discard unused variable" language server code action
  would produce invalid code when discarding left side assignment of string
  prefix patterns.
  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where the "Discard unused variable" language server code action
  would produce invalid code when discarding bindings in patterns with
  alternative patterns.
  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where the language server would not suggest prefix pattern
  prefix alias.
  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where on the JavaScript target a name shadowed inside a case
  clause that always matched would be used in place of the outer binding by the
  code that followed the case expression. This could produce a wrong value, or a
  runtime crash when the shadowed name was a module function or constant.
  ([John Downey](https://github.com/jtdowney))

- Fixed a bug where on the JavaScript target the variables a clause guard
  needed would be declared in the enclosing scope of a case that always
  matched, so a later `let` binding one of those names produced a duplicate
  declaration and the whole module failed to parse.
  ([John Downey](https://github.com/jtdowney))

## v1.18.1 - 2026-08-01

- Fixed a bug where the Erlang code generator would generate wrong code when
  referencing constants that are aliases to other constants in bit array
  segments, string concatenation and clause guards.
  ([Andrey Kozhev](https://github.com/ankddev))
