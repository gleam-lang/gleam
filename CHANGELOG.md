# Changelog

## Unreleased

### Compiler

 - The compiler now issues a friendlier error when attempting to pattern match
   on both the prefix and suffix of a string:

   ```
   error: Syntax error
     ┌─ /src/parse/error.gleam:2:23
     │
   2 │     "prefix" <> infix <> "suffix" -> infix
     │                       ^^^^^^^^^^^ This pattern is not allowed

   A string pattern can only match on a literal string prefix.

   Matching on a literal suffix is not possible, because `infix` would have an
   unknown size.
   ```

   ([Gavin Morrow](https://github.com/gavinmorrow))

- Let bindings now continue to assume their annotated type after mismatches,
  preventing misleading follow-up errors:

  ```gleam
  pub fn main() {
    let x: String = 5 // type error: expected String, got Int
    let y: String = x // valid
    let z: Int = x    // type error: expected Int, got String
  }
  ```

  ([Adi Salimgereyev](https://github.com/abs0luty))

### Build tool

- The build tool now generates Hexdocs URLs using the new format of
  `package.hexdocs.pm` rather than `hexdocs.pm/package`.
  ([Surya Rose](https://github.com/GearsDatapacks))

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
