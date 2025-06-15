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

- The formatter no longer removes empty lines between list items. In case an
  empty line is added between list items they will all be split on multiple
  lines.

  ```gleam
  pub fn main() {
    [
      "natu", "xatu",

      "chimeco"
    ]
  }
  ```

  Is formatted as:

  ```gleam
  pub fn main() {
    [
      "natu",
      "xatu",

      "chimeco",
    ]
  }
  ```

### Compiler

- The code generated for a `case` expression on the JavaScript target is now
  reduced in size in many cases.
  ([Surya Rose](https://github.com/GearsDatapacks))

- The code generators now perform usage-based dead code elimination. Unused
  definitions are not longer generated.
  ([Louis Pilfold](https://github.com/lpil))

- `echo` now has better support for character lists, JavaScript errors, and
  JavaScript circular references.
  ([Louis Pilfold](https://github.com/lpil))

### Build tool

- `gleam update`, `gleam deps update`, and `gleam deps download` will now print
  a message when there are new major versions of packages available.

  ```text
   $ gleam update
    Resolving versions

  The following dependencies have new major versions available:

  gleam_http 1.7.0 -> 4.0.0
  gleam_json 1.0.1 -> 3.0.1
  lustre     3.1.4 -> 5.1.1
  ```

  ([Amjad Mohamed](https://github.com/andho))


### Language server

### Formatter

### Bug fixes

- Fixed a bug where the language server would not show type-related code action
  for record fields in custom type definitions.
  ([cysabi](https://github.com/cysabi))

- Fixed a bug where the "Inline variable" code action would be offered for
  function parameters and other invalid cases.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the "Inline variable" code action would not be applied
  correctly to variables using label shorthand syntax.
  ([Surya Rose](https://github.com/GearsDatapacks))

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
