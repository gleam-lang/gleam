# Changelog

## Unreleased

### Compiler

### Build tool

- The `gleam dev` command now accepts the `--no-print-progress` flag. When this
  flag is passed, no progress information is printed.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The `gleam deps outdated` command now always prints a summary showing how many
  packages have newer versions available. For example:

  ```txt
  $ gleam deps outdated
  1 of 12 packages have newer versions available.

  Package       Current  Latest
  -------       -------  ------
  gleam_stdlib  0.70.0   0.71.0
  ```

  When no packages are outdated, only the summary line is printed:

  ```txt
  $ gleam deps outdated
  0 of 12 packages have newer versions available.
  ```

  ([Daniele Scaratti](https://github.com/lupodevelop))

### Language server

### Formatter

### Bug fixes

- Fixed a bug where the build tool would check for new major versions of a local
  or git dependency on Hex when running `gleam update`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
