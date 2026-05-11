# Changelog

## Unreleased

### Compiler

 - The compiler now issues a friendlier error when attempting to pattern match
   on both the prefix and suffix of a string:

   ```
   error: Syntax error
     ┌─ /src/match_string_end.gleam:8:13
     │
   8 │     "hi" <> b <> "c" -> todo
     │             ^ This must be a string literal prefix

   We can't tell what size this infix should be so we don't know
   how to handle this pattern.

   If you want to match one character consider using `pop_grapheme`
   from the stdlib's `gleam/string` module.
   ```

   ([Gavin Morrow](https://github.com/gavinmorrow))

### Build tool

- The build tool now generates Hexdocs URLs using the new format of
  `package.hexdocs.pm` rather than `hexdocs.pm/package`.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Language server

### Formatter

### Bug fixes
