# Changelog

## Unreleased

### Compiler

### Build tool

- The `gleam dev` command now accepts the `--no-print-progress` flag. When this
  flag is passed, no progress information is printed.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Erlang shipments now warn when run on a different OTP version than they were
  built with.
  ([Joris Hartog](https://github.com/nootr))

### Language server

### Formatter

### Bug fixes

- Fixed a bug where the build tool would check for new major versions of a local
  or git dependency on Hex when running `gleam update`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
