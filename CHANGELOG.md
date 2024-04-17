# Changelog

## Unreleased

### Build tool

- A helpful error message is now shown if the `manifest.toml` file has been
  edited to be invalid in some way. ([zahash](https://github.com/zahash))
  ```
  error: Corrupt manifest.toml

  The `manifest.toml` file is corrupt.

  Hint: Please fun `gleam update` to fix it.
  ```

### Compiler

- The compiler will now raise a warning for `let assert` assignments where the
  assertion is redundant.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
  ```
  warning: Redundant assertion
    ┌─ /home/lucy/src/app/src/app.gleam:4:7
    │
  4 │   let assert x = get_name()
    │       ^^^^^^ You can remove this

  This assertion is redundant since the pattern covers all possibilities.
  ```

- Empty case expressions are no longer parse errors and will instead be
  exhaustiveness errors. ([Race Williams](https://github.com/raquentin))

### Formatter

- Redundant alias names for imported modules are now removed.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
  ```gleam
  import gleam/result as result
  ```
  is formatted to
  ```gleam
  import gleam/result
  ```

### Bug Fixes

- Fixed [RUSTSEC-2021-0145](https://rustsec.org/advisories/RUSTSEC-2021-0145) by
  using Rust's `std::io::IsTerminal` instead of the `atty` library.
  ([Pi-Cla](https://github.com/Pi-Cla))

- Fixed the generated `mod` property in the Erlang application file when using the
  `application_start_module` property in `gleam.toml`.
  ([Alex Manning](https://github.com/rawhat))

- Fixed a bug where using some reserved keywords would result in confusing error
  messages. ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
