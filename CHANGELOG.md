# Changelog

## Unreleased

### Build tool

- The `--no-print-progress` flag has been added to prevent the build tool from
  printing messages as the project is built.
  ([Ankit Goel](https://github.com/crazymerlyn))

### Compiler

### Formatter

### Language Server

### Bug Fixes

- Fixed a bug which caused the language server and compiler to crash when
  two constructors of the same name were created.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where jumping to the definition of an unqualified function would
  produce the correct location, but remain in the same file.
  ([Surya Rose](https://github.com/gearsdatapacks))

- Fixed a bug where incorrect syntax error message were shown,
  when using `:` or `=` in wrong possitions in expressions.
  ([Ankit Goel](https://github.com/crazymerlyn))

- Fixed a bug where the compiler would crash when pattern matching on a type
  which had constructors of duplicate names.
  ([Surya Rose](https://github.com/gearsdatapacks))

## v1.4.1 - 2024-08-04

### Bug Fixes

- Fix a bug that caused record accessors for private types to not be completed
  by the LSP, even when in the same module.
  ([Ameen Radwan](https://github.com/Acepie))
