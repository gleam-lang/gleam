# Changelog

## Unreleased

### Compiler

- Improved code generation for blocks in tail position on the Javascript target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Function documentation comments and module documentation comments are now
  included in the generated Erlang code and can be browsed from the Erlang
  shell starting from OTP27.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

### Language server

### Formatter

### Bug fixes

- Fixed a bug where the "convert from use" code action would generate invalid
  code for use expressions ending with a trailing comma.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
