# Changelog


## Unreleased

### Compiler

### Build tool

### Language Server

### Formatter

### Bug fix

## v1.6.3 - 2024-12-03

### Bug fixed

- Fixed a bug where Gleam would be unable to compile to BEAM bytecode on older
  versions of Erlang/OTP.
  ([yoshi](https://github.com/joshi-monster))

## v1.6.2 - 2024-11-23

### Bug fixed

- Fixed a bug where patterns in `use` expressions would not be checked to ensure that
  they were exhaustive.
  ([Surya Rose](https://github.com/GearsDatapacks))

## v1.6.1 - 2024-11-19

### Bug fix

- Fixed a bug where `gleam update` would fail to update versions.
  ([Jason Sipula](https://github.com/SnakeDoc))
