# Changelog

## Unreleased

### Build tool

### Compiler

- Added more detail on the constructor error
  where a variable starting with an uppercase is defined by let binding.
  ([Arian Pourarian](https://github.com/Arian94))

### Formatter

### Language Server

### Bug Fixes

- Fixed a bug where the compiler could fail to detect modules that would clash
  with Erlang modules.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where dependency version resolution could crash for certain
  release candidate versions.
  ([Marshall Bowers](https://github.com/maxdeviant))

- Fixed a bug where trailing comments would be moved out of a bit array.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
