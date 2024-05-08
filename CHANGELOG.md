# Changelog

## v1.3.0 - Unreleased

### Build tool

- `gleam remove` will now present an error if a package being removed does not
  exist as a dependency in the project.
  ([Changfeng Lou](https://github.com/hnlcf))

### Compiler

- The compiler now emits a warning for redundant function captures in a
  pipeline:

  ```
  warning: Redundant function capture
    ┌─ /src/warning/wrn.gleam:5:17
    │
  5 │     1 |> wibble(_, 2) |> wibble(2)
    │                 ^ You can safely remove this

  This function capture is redundant since the value is already piped as the
  first argument of this call.

  See: https://tour.gleam.run/functions/pipelines/
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Functions etc named `maybe` are now escaped in generated Erlang as it is now a
  reserved word in Erlang/OTP 27.
  ([Jake Barszcz](https://github.com/barszcz))

- Non byte aligned arrays that use literals for size are now marked as an
  Unsupported feature for Javascript since they would already cause
  a runtime error on Javascript.

  This means if you compile specifically for Javascript you will now recieve this error:

  ```
  error: Unsupported feature for compilation target
    ┌─ /src/test/gleam_test.gleam:6:5
    │
  6 │   <<1:size(5)>>
    │     ^^^^^^^^^

  Non byte aligned array is not supported for JavaScript compilation.
  ```

  Else any functions which rely on this will not be compiled into Javascript.

### Formatter

### Language Server

- The language server will now suggest the "Remove redundant tuple" action even
  if the case expression contains some catch all patterns:

  ```
  case #(a, b) {
    #(1, 2) -> todo
    _ -> todo
  }
  ```

  Becomes:

  ```
  case a, b {
    1, 2 -> todo
    _, _ -> todo
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- LSP can now suggest completions for values and types from importable modules and adds the import to the top of the file.
  ([Ameen Radwan](https://github.com/Acepie)

- LSP completions now use the "text_edit" language server API resulting in better/more accurate insertions.
  ([Ameen Radwan](https://github.com/Acepie)

### Bug Fixes

- Fixed a bug where the compiler would output a confusing error message when
  trying to use the spread syntax to append to a list.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the formatter would strip empty lines out of the body of an
  anonymous function passed as an argument.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would crash when a type was defined with
  the same name as an imported type.
  ([Gears](https://github.com/gearsdatapacks))

- Fixed a bug where a horizontal scrollbar would appear on code blocks in built
  documentation when they contained lines 79 or 80 characters long.
  ([Richard Viney](https://github.com/richard-viney))

## v1.2.1 - 2024-05-30

### Bug Fixes

- Fixed a bug where the compiler could fail to detect modules that would clash
  with Erlang modules.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where dependency version resolution could crash for certain
  release candidate versions.
  ([Marshall Bowers](https://github.com/maxdeviant))

- Fixed a bug where trailing comments would be moved out of a bit array.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
