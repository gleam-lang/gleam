# Changelog

## Unreleased

### Formatter

- Function captures are now formatted like regular function calls.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## 1.7.0-rc2 - 2024-12-30

### Compiler

- Fixed a bug on JavaScript where a trailing `:bytes` segment would give the
  wrong pattern match result for a sliced bit array.
  ([Richard Viney](https://github.com/richard-viney))

## 1.7.0-rc1 - 2024-12-29

### Compiler

- Removed compiler hint about pattern matching a `Result(a, b)` when being used
  where `a` is expected.
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

- A custom panic message can now be specified when asserting a value with
  `let assert`:

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

- Better error message for existed type constructor being used as value
  constructor.
  ([Jiangda Wang](https://github.com/Frank-III))

- Print better error messages when shell commands used by compiler cannot be found.
  ([wheatfox](https://github.com/enkerewpo))

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

- `gleam publish` now warns when publishing packages that define multiple
  top-level modules, as this can lead to namespace pollution and conflicts for
  consumers.
  ([Aleksei Gurianov](https://github.com/guria))

- New projects now require `gleam_stdlib` v0.44.0.
  ([Louis Pilfold](https://github.com/lpil))

- `gleam remove` no longer requires a network connection.
  ([yoshi](https://github.com/joshi-monster))

- Commands that work with the Hex package manager API now create and store an
  API key rather than creating a new one each time. This API key is encrypted
  with a local password, reducing risk of your Hex password being compromised.
  ([Louis Pilfold](https://github.com/lpil))

- The build tool now sets the `REBAR_SKIP_PROJECT_PLUGINS` environment variable
  when using rebar3 to compile Erlang dependencies. With future versions of
  rebar3 this will cause it to skip project plugins, significantly reducing the
  amount of code it'll need to download and compile, improving compile times.
  ([Tristan Sloughter](https://github.com/tsloughter))

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

- The language server now provides an action to extract a value into a variable.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The Language Server now suggests a code action to expand a function capture
  into the equivalent anonymous function. For example, this snippet of code:

  ```gleam
  pub fn main() {
    list.map([1, 2, 3], int.add(_, 11))
  }
  ```

  Will be turned into:

  ```gleam
  pub fn main() {
    list.map([1, 2, 3], fn(value) { int.add(value, 11) })
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now suggests a code action to generate a dynamic decoder
  for a custom type. For example, this code:

  ```gleam
  pub type Person {
    Person(name: String, age: Int)
  }
  ```

  Will become:

  ```gleam
  import gleam/dynamic/decode

  pub type Person {
    Person(name: String, age: Int)
  }

  fn person_decoder() -> decode.Decoder(Person) {
    use name <- decode.field("name", decode.string)
    use age <- decode.field("age", decode.int)

    decode.success(Person(name:, age:))
  }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

### Formatter

- The formatter now adds a `todo` inside empty blocks.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Documentation

- Canonical links created for documentation pages if published on Hex.
  Previously published documentation would need to be updated.
  Resolves versioned pages to point to latest page for search engines.
  ([Dave Lage](https://github.com/rockerBOO))

- The formatter now format lists the same in constants as in expressions
  ([Jiangda Wang](https://github.com/Frank-III))

### Bug fixed

- The compiler now throws an error when a float literal ends with an `e` and
  is missing an exponent.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a crash with ENOTEMPTY (os error 39) when building on NTFS partitions
  ([Ivan Ermakov](https://github.com/ivanjermakov))

- Fixed a bug where the compiler would crash when pattern matching on multiple
  subjects and one of them being a constant record.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Variant inference on prelude types now works correctly if the variant is
  constant.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where patterns in `use` expressions would not be checked to ensure
  that they were exhaustive.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where a `module.mjs` file would be overwritten by a `module.gleam`
  file of same name without warning. It now produces an error.
  ([PgBiel](https://github.com/PgBiel))

- Modules depending on removed or renamed modules now get automatically
  recompiled.
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

- Fixed a bug where the inferred variant of values was not properly cached,
  leading to incorrect errors on incremental builds and in the Language Server.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where Gleam would be unable to compile to BEAM bytecode if the
  project path contains a non-ascii character.
  ([yoshi](https://github.com/joshi-monster))

- Fixed a bug where the compiler would display incorrect hints about ignoring
  unused variables in certain cases.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the completer would not include braces in type import completions
  when it should have.
  ([Jiangda Wang](https://github.com/Frank-III))

- Fixed a bug where `gleam new` would generate the github `test` workflow
  configuration with incompatible Erlang OTP and Elixir versions.
  ([John Strunk](https://github.com/jrstrunk))

## v1.6.1 - 2024-11-19

### Bug fixed

- Fixed a bug where `gleam update` would fail to update versions.
  ([Jason Sipula](https://github.com/SnakeDoc))
