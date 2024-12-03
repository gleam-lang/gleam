# Changelog

## Unreleased

### Compiler

- Removed compiler hint about pattern matching a `Result(a, b)` when being used where `a` is expected.
  ([Kieran O'Reilly](https://github.com/SoTeKie))

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

- A custom panic message can now be specified when asserting a value with `let assert`:

  ```gleam
  let assert Ok(regex) = regex.compile("ab?c+") as "This regex is always valid"
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- When targeting JavaScript the compiler now generates faster and smaller code
  for `Int` values in bit array expressions and patterns by evaluating them at
  compile time where possible.
  ([Richard Viney](https://github.com/richard-viney))

- Qualified records can now be used in clause guards.
  ([Surya Rose](https://github.com/GearsDatapacks))

- The compiler now allows deprecating specific custom type variants using the
  `@deprecated` attribute:

  ```gleam
  pub type HashAlgorithm {
    @deprecated("Please upgrade to another algorithm")
    Md5
    Sha224
    Sha512
  }

  pub fn hash_password(input: String) -> String {
    hash(input:, algorithm: Md5) // Warning: Deprecated value used
  }
  ```

  ([Iesha](https://github.com/wilbert-mad))

- On the JavaScript target, taking byte-aligned slices of bit arrays is now an
  O(1) operation instead of O(N), significantly improving performance.
  ([Richard Viney](https://github.com/richard-viney))

### Build tool

- Improved the error message you get when trying to add a package that doesn't
  exist with `gleam add`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- External files (such as `.mjs` and `.erl`) are now permitted in subdirectories
  of `src/` and `test/`.
  ([PgBiel](https://github.com/PgBiel))

- `gleam publish` now requires more verbose confirmation for publishing Gleam
  team packages and v0 packages.
  ([Louis Pilfold](https://github.com/lpil))

- `gleam publish` now warns when publishing packages that define multiple top-level
  modules, as this can lead to namespace pollution and conflicts for consumers.
  ([Aleksei Gurianov](https://github.com/guria))

- New projects now require `gleam_stdlib` v0.44.0.

- `gleam remove` no longer requires a network connection.
  ([yoshi](https://github.com/joshi-monster))

- Commands that work with the Hex package manager API now create and store an
  API key rather than creating a new one each time. This API key is encrypted
  with a local password, reducing risk of your Hex password being compromised.
  ([Louis Pilfold](https://github.com/lpil))

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

- The language server now suggests a code action to convert an inexhaustive
  `let` assignment into a `case` expression:

  ```gleam
  pub fn unwrap_result(result: Result(a, b)) -> a {
    let Ok(inner) = result
    inner
  }
  ```

  Becomes:

  ```gleam
  pub fn unwrap_result(result: Result(a, b)) -> a {
    let inner = case result {
      Ok(inner) -> inner
      Error(_) -> todo
    }
    inner
  }
  ```

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

- Modules depending on removed or renamed modules now get automatically recompiled.
  ([Sakari Bergen](https://github.com/sbergen))

- The compiler now raises a warning for unused case expressions, code blocks and
  pipelines that would be safe to remove.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

 - Fixed a bug where assigning the prefix of a string pattern to a variable
   nested inside another pattern would produce invalid code on Javascript.
   ([yoshi](https://github.com/joshi-monster))

- Fixed a bug where expressions which use an unsafe integer on JavaScript would
  not emit a warning if an @external function had been referenced.
  ([Richard Viney](https://github.com/richard-viney))

- Fixed a bug where nested tuple access would not be parsed correctly when
  the left-hand side was a function call.
  ([Surya Rose](https://github.com/GearsDatapacks))

 - Fixed a bug where Gleam would be unable to compile to BEAM bytecode on older
   versions of Erlang/OTP.
   ([yoshi](https://github.com/joshi-monster))

## v1.6.1 - 2024-11-19

### Bug fixed

- Fixed a bug where `gleam update` would fail to update versions.
  ([Jason Sipula](https://github.com/SnakeDoc))
