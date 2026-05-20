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

   Matching on a literal suffix is not possible, because `infix` would have an unknown size.
   ```

   ([Gavin Morrow](https://github.com/gavinmorrow))

### Build tool

- The build tool now generates Hexdocs URLs using the new format of
  `package.hexdocs.pm` rather than `hexdocs.pm/package`.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Language server

### Formatter

### Bug fixes
