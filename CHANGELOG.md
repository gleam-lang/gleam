# Changelog

## Unreleased

### Compiler

- Optimised code generated for record updates.
  ([yoshi](https://github.com/joshi-monster))

- The compiler now allows for record updates to change the generic type
  parameters of the record:

  ```gleam
  type Box(value) {
    Box(password: String, value: value)
  }

  fn insert(box: Box(a), value: b) -> Box(b) {
    Box(..box, value:)
  }
  ```

  ([yoshi](https://github.com/joshi-monster))

- It is now allowed to write a block with no expressions. Like an empty function
  body, an empty block is considered incomplete as if it contained a `todo`
  expression.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The shorthand names for the two targets, `erl` and `js` are now
  deprecated in code such as `@target`.
  ([Surya Rose](https://github.com/GearsDatapacks))

- When targeting JavaScript the compiler now generates faster and smaller code
  for `Int` values in bit array expressions and patterns by evaluating them at
  compile time where possible.
  ([Richard Viney](https://github.com/richard-viney))

### Build tool

- Improved the error message you get when trying to add a package that doesn't
  exist with `gleam add`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- FFI files (such as `.mjs` and `.erl`) are now permitted in subdirectories of
  `src/` and `test/`.
  ([PgBiel](https://github.com/PgBiel))

### Language Server

- The language server now provides type information when hovering over argument
  labels.
  ([Surya Rose](https://github.com/GearsDatapacks))

- The Language Server now suggests a code action to desugar a use expression
  into the equivalent function call. For example, this snippet of code:

  ```gleam
  pub fn main() {
    use profile <- result.try(fetch_profile(user))
    render_welcome(user, profile)
  }
  ```

  Will be turned into:

  ```gleam
  pub fn main() {
    result.try(fetch_profile(user), fn(profile) {
      render_welcome(user, profile)
    })
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The Language Server now suggests a code action to turn a function call into
  the equivalent use expression. For example, this snippet of code:

  ```gleam
  pub fn main() {
    result.try(fetch_profile(user) fn(profile) {
      render_welcome(user, profile)
    })
  }
  ```

  Will be turned into:

  ```gleam
  pub fn main() {
    use profile <- result.try(fetch_profile(user))
    render_welcome(user, profile)
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now provides correct information when hovering over
  patterns in use expressions.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Formatter

- The formatter now adds a `todo` inside empty blocks.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug fixed

- The compiler now throws an error when a float literal ends with an `e` and
  is missing an exponent.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a crash with ENOTEMPTY (os error 39) when building on NTFS partitions
  ([Ivan Ermakov](https://github.com/ivanjermakov))

- Fixed a bug where the compiler would crash when pattern matching on multiple
  subjects and one of them being a constant record.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Variant inference on prelude types now works correctly if the variant is constant.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where patterns in `use` expressions would not be checked to ensure that
  they were exhaustive.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where a `module.mjs` file would be overwritten by a `module.gleam`
  file of same name without warning. It now produces an error.
  ([PgBiel](https://github.com/PgBiel))

## v1.6.1 - 2024-11-19

### Bug fixed

- Fixed a bug where `gleam update` would fail to update versions.
  ([Jason Sipula](https://github.com/SnakeDoc))
