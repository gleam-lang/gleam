# Changelog

## Unreleased

### Compiler

- The compiler now supports list prepending in constants. For example:

  ```gleam
  pub const viviparous_mammals = ["dog", "cat", "human"]

  pub const all_mammals = ["platypus", "echidna", ..viviparous_mammals]
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- The analysis of record update expressions is now fault tolerant, meaning the
  compiler will no longer stop reporting errors at the first invalid field it
  finds.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now shows a better error message when trying to use the record
  update syntax with variants that have no labelled fields.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now raises a warning on the JavaScript target when defining an
  integer segment with a size higher than 52 bits. For example, this code:

  ```gleam
  pub fn go(sha: BitArray) {
      let <<_, number:152>> = sha
      number
  }
  ```

  Will result in the following warning:

  ```txt
  warning: Truncated bit array segment
    ┌─ /src/warning/wrn.gleam:3:20
    │
  3 │     let <<_, number:152>> = sha
    │                     ^^^

  This segment is a 152-bit long integer, but on the JavaScript target
  numbers have at most 52 bits. It would be truncated to its first 52 bits.
  Hint: Did you mean to use the `bytes` segment option?
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

- When publishing, the package manager now uses the full term instead of the
  shorthand "MFA" in the prompt and error message.
  ([Luka Ivanović](https://github.com/luka-hash))

- When Hex rejects publish with error 422, show error message instead of
  defaulting to "can only modify a release up to one hour after publication"
  ([David Matz](https://github.com/d-matz))

### Language server

- The "extract variable" code action can now pick better names for variables in
  case branches and blocks, ignoring unrelated names of variables in other
  branches.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now has a code action to replace a `_` in a type
  annotation with the corresponding type. For example:

  ```gleam
  pub fn load_user(id: Int) -> Result(_, Error) {
    //                                ^
    //      Triggering the code action here
    sql.find_by_id(id)
    |> result.map_error(CannotLoadUser)
  }
  ```

  Triggering the code action over the `_` will result in the following code:

  ```gleam
  pub fn load_user(id: Int) -> Result(User, Error) {
    sql.find_by_id(id)
    |> result.map_error(CannotLoadUser)
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now shows completions for the labelled argument of a
  record when writing a record update.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

- The formatter no longer moves comments out of type annotations.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug fixes

- Fixed a bug where the compiler would crash when trying to read the cache for
  modules containing large constants.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where `BitArray$BitArray$data` constructed a `DataView` with
  incorrect byte length instead of the slice's actual size, causing sliced bit
  arrays to include extra bytes from the underlying buffer on JavaScript.
  ([John Downey](https://github.com/jtdowney))

- The compiler now parses UTF-8 source files with a byte-order mark correctly,
  instead of raising an error.
  ([Lucy McPhail](https://github.com/lucymcphail))

- Fixed a bug where semicolons would not be properly added to pipelines in
  generated JavaScript code, leading to runtime errors in certain circumstances.
  ([Surya Rose](https://github.com/GearsDatapacks))

## v1.15.1 - 2026-03-17

### Bug fixes

- Fixed a bug where `BitArray$BitArray$data` constructed a `DataView` with
  offset 0 instead of the slice's actual byte offset, causing sliced bit arrays
  to read from the wrong position in the underlying buffer on JavaScript.
  ([John Downey](https://github.com/jtdowney))

- Fixed a bug where the "Add missing type parameter" code action could be
  triggered on types that do not exist instead of type variables.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
