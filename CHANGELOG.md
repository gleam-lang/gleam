# Changelog

## Unreleased

### Formatter

- The formatter now allows more control over how lists are split. By adding a
  trailing comma at the end of a list that can fit on a single line, the list
  will be split on multiple lines:

  ```gleam
  pub fn my_favourite_pokemon() -> List(String) {
    ["natu", "chimecho", "milotic",]
  }
  ```

  Will be formatted as:

  ```gleam
  pub fn my_favourite_pokemon() -> List(String) {
    [
      "natu",
      "chimecho",
      "milotic",
    ]
  }
  ```

  By removing the trailing comma, the formatter will try and fit the list on a
  single line again:

  ```gleam
  pub fn my_favourite_pokemon() -> List(String) {
    [
      "natu",
      "chimecho",
      "milotic"
    ]
  }
  ```

  Will be formatted back to a single line:

  ```gleam
  pub fn my_favourite_pokemon() -> List(String) {
    ["natu", "chimecho", "milotic"]
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The formatter now allows more control over how lists made up of simple
  constants are formatted. If a list is split with multiple elements on the same
  line, removing the trailing comma will make sure the formatter keeps each item
  on its own line:

  ```gleam
  pub fn my_favourite_pokemon() -> List(String) {
    [
      "This list was formatted", "keeping multiple elements on the same line",
      "notice how the formatting changes by removing the trailing comma ->"
    ]
  }
  ```

  Is formatted as:

  ```gleam
  pub fn my_favourite_pokemon() -> List(String) {
    [
      "This list was formatted",
      "keeping multiple elements on the same line",
      "notice how the formatting changes by removing the trailing comma ->",
    ]
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Compiler

### Build tool

### Language server

### Formatter

### Bug fixes

- Fixed a bug where the language server would not show type-related code action
  for record fields in custom type definitions.
  ([cysabi](https://github.com/cysabi))

## v1.11.1 - 2025-06-05

### Compiler

- The displaying of internal types in HTML documentation has been improved.
  ([Louis Pilfold](https://github.com/lpil))

- A warning is now emitted when the same module is imported
  multiple times into the same module with different aliases.
  ([Louis Pilfold](https://github.com/lpil))

### Bug fixes

- Fixed a bug where a bit array segment matching on a floating point number
  would match with `NaN` or `Infinity` on the JavaScript target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
