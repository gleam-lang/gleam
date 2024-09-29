# Changelog

## Unreleased

### Build tool

- The `--template` flag for `gleam new` takes the values `erlang` and
  `javascript` to specify what target to use, with `erlang` being the default.
  ([Mohammed Khouni](https://github.com/Tar-Tarus))

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

### Bug Fixes

- Fixed a bug in the compiler where shadowing a sized value in a bit pattern
  would cause invalid erlang code to be generated.
  ([Antonio Iaccarino](https://github.com/eingin))

- Fixed a bug where the formatter would not format strings with big grapheme
  clusters properly.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.5.1 - 2024-09-26

### Bug Fixes

- Fixed a bug where Erlang file paths would not be escaped on Windows.
  ([Louis Pilfold](https://github.com/lpil))
