# Changelog

## Unreleased

### Compiler

### Build tool

### Language server

### Formatter

### Bug fixes

- When using the language server to extract a function from within an anonymous
  function, the return value of the extracted function is respected.

  For example,
  ```gleam
  fn wibble() {
    let wobble = fn() {
      let random_number = 4
      random_number * 42 // <- Extracting this line
    }
  }
  ```
  is turned into
  ```gleam
  fn wibble() {
    let wobble = fn() {
      let random_number = 4
      function(random_number)
    }
  }

  fn function(random_number: Int) -> Int {
    random_number * 42
  }
  ```

  [Gavin Morrow](https://github.com/gavinmorrow)
