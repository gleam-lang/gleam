# Changelog

## Unreleased

### Compiler

### Build tool

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

### Formatter

### Bug fixes

- Fixed a bug where the compiler would crash when trying to read the cache for
  modules containing large constants.
  ([Surya Rose](https://github.com/GearsDatapacks))

## v1.15.1 - 2026-03-17

### Bug fixes

- Fixed a bug where `BitArray$BitArray$data` constructed a `DataView` with
  offset 0 instead of the slice's actual byte offset, causing sliced bit arrays
  to read from the wrong position in the underlying buffer on JavaScript.
  ([John Downey](https://github.com/jtdowney))
