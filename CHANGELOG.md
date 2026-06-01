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

### Build tool

- The build tool now generates Hexdocs URLs using the new format of
  `package.hexdocs.pm` rather than `hexdocs.pm/package`.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Language server

- The language server now has "Discard unused variable" code action to discard
  unused variables in different places. For example,

  ```gleam
  pub type Wibble {
    Wibble(a: Int)
  }

  pub fn go(record: Wibble) -> Nil {
    let wibble = 0
    //  ^ Trigger code action here

    case record {
      Wibble(a:) -> Nil
      //     ^ Trigger code action here
    }

    case [0, 1, 2] {
      [_, ..] as wobble -> Nil
      //         ^ Trigger code action here
      [] -> Nil
    }

    Nil
  }
  ```

  Triggering the code action in all of these places would produce following code:

  ```gleam
  pub type Wibble {
    Wibble(a: Int)
  }

  pub fn go(record: Wibble) -> Nil {
    let _wibble = 0

    case record {
      Wibble(a: _) -> Nil
    }

    case [0, 1, 2] {
      [_, ..] -> Nil
      [] -> Nil
    }

    Nil
  }
  ```

  ([Andrey Kozhev](https://github.com/ankddev))

### Formatter

### Bug fixes
