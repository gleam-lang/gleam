# Changelog

## Unreleased

- The language server now offers code actions to wrap a function reference in an
  anonymous function, or to remove a trivial anonymous function, leaving its
  contents. For example:

  ```gleam
  pub fn main() {
    [-1, -2, -3] |> list.map(fn(a) { int.absolute_value(a) })
                          // ^^ Activating the "Remove anonymous function"
                          // code action here
  }
  ```

  would result in:

  ```gleam
  pub fn main() {
    [-1, -2, -3] |> list.map(int.absolute_value)
  }
  ```

  while the other action would reverse the change. ([Eli Treuherz](http.github.com/treuherz))

### Compiler

### Build tool

### Language server

- The "extract variable" code action can now pick better names for variables in
  case branches and blocks, ignoring unrelated names of variables in other
  branches.
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
