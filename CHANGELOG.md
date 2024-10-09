# Changelog

## Unreleased

### Build tool

- The `--template` flag for `gleam new` takes the values `erlang` and
  `javascript` to specify what target to use, with `erlang` being the default.
  ([Mohammed Khouni](https://github.com/Tar-Tarus))

- The Erlang/Elixir compiler process is now re-used for all packages, shaving
  off 0.3-0.5s per compiled package.
  ([yoshi](https://github.com/joshi-monster))

### Compiler

- The compiler now prints correctly qualified or aliased type names when
  printing type errors.

  This code:

  ```gleam
  pub type Int

  pub fn different_int_types(value: Int) {
    value
  }

  pub fn main() {
    different_int_types(20)
  }
  ```

  Produces this error:

  ```
  error: Type mismatch
    ┌─ /src/wibble.gleam:8:23
    │
  8 │   different_int_types(20)
    │                       ^^

  Expected type:

      Int

  Found type:

      gleam.Int
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- The compiler can now suggest to pattern match on a `Result(a, b)` if it's
  being used where a value of type `a` is expected. For example, this code:

  ```gleam
  import gleam/list
  import gleam/int

  pub fn main() {
    let not_a_number = list.first([1, 2, 3])
    int.add(1, not_a_number)
  }
  ```

  Results in the following error:

  ```txt
  error: Type mismatch
    ┌─ /src/one/two.gleam:6:9
    │
  6 │   int.add(1, not_a_number)
    │              ^^^^^^^^^^^^

  Expected type:

      Int

  Found type:

      Result(Int, a)

  Hint: If you want to get a `Int` out of a `Result(Int, a)` you can pattern
  match on it:

      case result {
        Ok(value) -> todo
        Error(error) -> todo
      }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Improved the error message for unknown record fields, displaying an additional
  note on how to have a field accessor only if it makes sense.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now ignores `optional` dependencies when resolving versions
  unless explicitly specified.
  ([Gustavo Inacio](https://github.com/gusinacio))

- Fixes a bug where incorrect code would be generated for external function on
  the Erlang target if any of their arguments were discarded.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

### Language Server

- The Language Server now displays correctly qualified or aliased type names
  when hovering over a value in a Gleam file:

  ```gleam
  import gleam/option

  const value = option.Some(1)
  //    ^ hovering here shows `option.Option(Int)`
  ```

  ```gleam
  import gleam/option.{type Option as Maybe}

  const value = option.Some(1)
  //    ^ hovering here shows `Maybe(Int)`
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- The Language Server now suggests a code action to add type annotations to
  local variables, constants and functions:

  ```gleam
  pub fn add_int_to_float(a, b) {
    a +. int.to_float(b)
  }
  ```

  Becomes:

  ```gleam
  pub fn add_int_to_float(a: Float, b: Int) -> Float {
    a +. int.to_float(b)
  }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

### Bug Fixes

- Fixed a bug in the compiler where shadowing a sized value in a bit pattern
  would cause invalid erlang code to be generated.
  ([Antonio Iaccarino](https://github.com/eingin))

- Fixed a bug where the formatter would not format strings with big grapheme
  clusters properly.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed the `BitArray` constructor not being present in the types for the
  JavaScript prelude.
  ([Richard Viney](https://github.com/richard-viney))

- Fixed a bug where generated TypeScript definitions were invalid for opaque
  types that use a private type.
  ([Richard Viney](https://github.com/richard-viney))

## v1.5.1 - 2024-09-26

### Bug Fixes

- Fixed a bug where Erlang file paths would not be escaped on Windows.
  ([Louis Pilfold](https://github.com/lpil))
