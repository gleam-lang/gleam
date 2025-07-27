# Changelog

## Unreleased

### Compiler

- The compiler now suggest public values from imported modules when the variable
  in unknown. These values are suggested based on name and arity.

  Considering this program:

  ```gleam
  import gleam/io

  pub fn main() -> Nil {
    println("Hello, World!")
  }
  ```

  The compiler will display this error message:
  ```text
    error: Unknown variable
    ┌─ /path/to/project/src/project.gleam:4:3
    │
  4 │   println("Hello, World!")
    │   ^^^^^^^

  The name `println` is not in scope here.
  Did you mean one of these:

      - io.println
  ```

  ([raphrous](https://github.com/realraphrous))

### Build tool

- The `gleam dev` command now accepts the `--no-print-progress` flag. When this
  flag is passed, no progress information is printed.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Language server

### Formatter

### Bug fixes

- Fixed a bug where the build tool would check for new major versions of a local
  or git dependency on Hex when running `gleam update`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
