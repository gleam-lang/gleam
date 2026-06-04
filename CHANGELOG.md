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

- Improved the error message shown when using an invalid discard name for
  functions, constants, module names, and `as` patterns.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

- The build tool now generates Hexdocs URLs using the new format of
  `package.hexdocs.pm` rather than `hexdocs.pm/package`.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Language server

- The "pattern match on value" code action can now be used to pattern match on
  values returned by function calls. For example:

  ```gleam
  pub fn main() {
    load_user()
  // ^^ Triggering the code action over here
  }

  fn load_user() -> Result(User, Nil) { todo }
  ```

  Will produce the following code:

  ```gleam
  pub fn main() {
    case load_user() {
      Ok(value) -> todo
      Error(value) -> todo
    }
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

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

  ([Gavin Morrow](https://github.com/gavinmorrow))

- Work around an ambiguity of the language server protocol that resulted in
  editors like Zed inserting the wrong text when accepting type completions.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would raise a warning for truncated int
  segments when compiling a function with a JavaScript external.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
