# Changelog

## Unreleased

### Compiler

### Build tool

- Git dependencies now support an optional `path` field to specify a
  subdirectory within the repository. This is useful for monorepos that
  contain multiple Gleam packages. For example:

  ```toml
  [dependencies]
  my_package = { git = "https://github.com/example/monorepo", ref = "main", path = "packages/my_package" }
  ```

  ([John Downey](https://github.com/jtdowney))

### Language server

### Formatter

### Bug fixes
