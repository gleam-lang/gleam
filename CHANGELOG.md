# Changelog

## Unreleased

### Compiler

### Build tool

- New projects are generated using OTP28 on GitHub Actions.
  ([Louis Pilfold](https://github.com/lpil))

- `gleam add` now adds `dependencies` and `dev-dependencies` as tables instead
  of inline tables if they are missing.
  ([Andrey Kozhev](https://github.com/ankddev))

- `gleam run -m <module>` now calls the requested module’s `main` function
  directly rather than first starting the project’s `erlang.application_start_module`.
    
  This prevents crashes when the start module isn’t meant to be run in that
  context.

  ([Aayush Tripathi](https://github.com/aayush-tripathi))

### Language server

### Formatter

### Bug fixes

- Fixed a bug where `echo` could crash on JavaScript if the module contains
  record variants with the same name as some built-in JavaScript objects.
  ([Louis Pilfold](https://github.com/lpil))
