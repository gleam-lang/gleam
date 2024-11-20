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

### Build tool

- Improved the error message you get when trying to add a package that doesn't
  exist with `gleam add`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

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

### Formatter

- The formatter now adds a `todo` inside empty blocks.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug fixed

- The compiler now throws an error when a float literal ends with an `e` and
  is missing an exponent.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the compiler would crash when pattern matching on multiple
  subjects and one of them being a constant record.
  ([Surya Rose](https://github.com/GearsDatapacks))

## v1.6.1 - 2024-11-19

### Bug fixed

- Fixed a bug where `gleam update` would fail to update versions.
