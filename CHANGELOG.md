# Changelog

## v1.12.0-rc3 - 2025-07-31

### Bug fixes

- Fixed a bug where using `echo` in a module with a function named `process`
  would result in a runtime error on JavaScript.
  ([Peter Saxton](https://github.com/CrowdHailer))

## v1.12.0-rc2 - 2025-07-24

### Formatter

- The formatter now allows more control over how bit arrays are split. By adding
  a trailing comma at the end of a bit array that can fit on a single line, the
  bit array will be split on multiple lines:

  ```gleam
  pub fn dgram() -> BitArray {
    <<ip_version:4, header_length:4, service_type:8,>>
  }
  ```

  Will be formatted as:

  ```gleam
  pub fn dgram() -> BitArray {
    <<
      ip_version:4,
      header_length:4,
      service_type:8,
    >>
  }
  ```

  By removing the trailing comma, the formatter will try and fit the bit array
  on a single line again:

  ```gleam
  pub fn dgram() -> BitArray {
    <<
      ip_version:4,
      header_length:4,
      service_type:8
    >>
  }
  ```

  Will be formatted back to a single line:

  ```gleam
  pub fn dgram() -> BitArray {
    <<ip_version:4, header_length:4, service_type:8>>
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The formatter now allows more control over how bit arrays are formatted.
  If a bit array is split with multiple segments on the same line, removing the
  trailing comma will make sure the formatter keeps each segment on its own
  line:

  ```gleam
  pub fn dgram() -> BitArray {
    <<
      "This bit array was formatted", "keeping segments on the same line",
      "notice how the formatting changes by removing the trailing comma ->",
    >>
  }
  ```

  Is formatted as:

  ```gleam
  pub fn dgram() -> BitArray {
    <<
      "This bit array was formatted",
      "keeping segments on the same line",
      "notice how the formatting changes by removing the trailing comma ->"
    >>
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug fixes

- Fixed a bug where the formatter would move a comment before `assert` to be
  after it.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the message following an `echo`, `panic`, `todo`, `assert`,
  or `let assert` would not be formatted properly when preceded by a comment.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would generate invalid code for an `assert`
  using pipes on the JavaScript target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.12.0-rc1 - 2025-07-18

### Compiler

- It is now possible to add a custom message to be printed by `echo`, making it
  easier to include additional context to be printed at runtime:

  ```gleam
  pub fn main() {
    echo 11 as "lucky number"
  }
  ```

  Will output to stderr:

  ```txt
  /src/module.gleam:2 lucky number
  11
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Generated JavaScript functions, constants, and custom type constructors now
  include any doc comment as a JSDoc comment, making it easier to use the
  generated code and browse its documentation from JavaScript.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The code generated for a `case` expression on the JavaScript target is now
  reduced in size in many cases.
  ([Surya Rose](https://github.com/GearsDatapacks))

- The code generators now perform usage-based dead code elimination. Unused
  definitions are not longer generated.
  ([Louis Pilfold](https://github.com/lpil))

- `echo` now has better support for character lists, JavaScript errors, and
  JavaScript circular references.
  ([Louis Pilfold](https://github.com/lpil))

- The look of errors and warnings has been improved. Additional labels providing
  context for the error message are no longer highlighted with the same style as
  the source of the problem.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Gleam will now emit a helpful message when attempting to import modules using
  `.` instead of `/`.

  ```txt
  error: Syntax error
    ┌─ /src/parse/error.gleam:1:11
    │
  1 │ import one.two.three
    │           ^ I was expecting either `/` or `.{` here.

  Perhaps you meant one of:

      import one/two
      import one.{item}
  ```

  ([Zij-IT](https://github.com/zij-it))

- The compiler now emits a warning when a top-level constant or function
  declaration shadows an imported name in the current module.
  ([Aayush Tripathi](https://github.com/aayush-tripathi))

- The compiler can now tell when an unknown variable might be referring to an
  ignored variable and provide an helpful error message highlighting it. For
  example, this piece of code:

  ```gleam
  pub fn go() {
    let _x = 1
    x + 1
  }
  ```

  Now results in the following error:

  ```
  error: Unknown variable
    ┌─ /src/one/two.gleam:4:3
    │
  3 │   let _x = 1
    │       -- This value is discarded
  4 │   x + 1
    │   ^ So it is not in scope here.

  Hint: Change `_x` to `x` or reference another variable
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The code generated for pattern matching has been optimised on the JavaScript
  target to reuse the matched variables when safe to do so. Take the following
  snippet of Gleam code:

  ```gleam
  pub fn find_book() {
    case ask_for_isbn() {
      Ok(isbn) -> load_book(isbn)
      Error(Nil) -> Error(Nil)
    }
  }
  ```

  Notice how in the `Error` case we're returning exactly the same value that is
  being matched on! Now the compiler will generate the following JavaScript code
  instead of allocating a new `Error` variant entirely:

  ```js
  export function find_book() {
    let result = ask_for_isbn();
    if (result instanceof Ok) {
      let isbn = result[0];
      return load_book(isbn);
    } else {
      // Previously this would have been: `return new Error(undefined);`!
      return result;
    }
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now raises a warning when performing a redundant comparison that
  it can tell is always going to succeed or fail. For example, this piece of
  code:

  ```gleam
  pub fn find_line(lines) {
    list.find(lines, fn(x) { x == x })
  }
  ```

  Would result in the following warning:

  ```
  warning: Redundant comparison
    ┌─ /src/warning.gleam:2:17
    │
  1 │   list.find(lines, fn(x) { x == x })
    │                            ^^^^^^ This is always `True`

  This comparison is redundant since it always succeeds.
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- When using two spreads, trying to concatenate lists, the compiler will now
  show a nicer error message. For example, this snippet of code:

  ```gleam
  pub fn main() -> Nil {
    let xs = [1, 2, 3]
    let ys = [5, 6, 7]
    [1, ..xs, ..ys]
  }
  ```

  Would result in the following error:

  ```
  error: Syntax error
    ┌─ /src/parse/error.gleam:5:13
    │
  5 │   [1, ..xs, ..ys]
    │       --    ^^ I wasn't expecting a second spread here
    │       │
    │       You're using a spread here

  Lists are immutable and singly-linked, so to join two or more lists
  all the elements of the lists would need to be copied into a new list.
  This would be slow, so there is no built-in syntax for it.
  ```

  ([Carl Bordum Hansen](https://github.com/carlbordum)) and
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The error message one gets when calling a function with the wrong number of
  arguments has been improved and now only suggests the relevant missing labels.
  For example, this piece of code:

  ```gleam
  pub type Pokemon {
    Pokemon(id: Int, name: String, moves: List(String))
  }

  pub fn best_pokemon() {
    Pokemon(198, name: "murkrow")
  }
  ```

  Would result in the following error, suggesting the missing labels:

  ```txt
  error: Incorrect arity
    ┌─ /src/main.gleam:6:3
    │
  6 │   Pokemon(198, name: "murkrow")
    │   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Expected 3 arguments, got 2

  This call accepts these additional labelled arguments:

    - moves
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Code generators now reuse existing variables when possible for the record
  update syntax, reducing the size of the generated code and number of
  variables defined for both Erlang and JavaScript.

  ```gleam
  pub fn main() -> Nil {
    let trainer = Trainer(name: "Ash", badges: 0)
    battle(Wobble(..trainer, badges: 1))
  }
  ```

  Previously this Gleam code would generate this Erlang code:

  ```erlang
  -spec main() -> nil.
  main() ->
      Trainer = {trainer, 0, <<"Ash"/utf8>>},
      battle(
          begin
              _record = Trainer,
              {trainer, 1, erlang:element(3, _record)}
          end
      ).
  ```

  Now this code will be generated instead:

  ```erlang
  -spec main() -> nil.
  main() ->
      Trainer = {trainer, 0, <<"Ash"/utf8>>},
      battle({trainer, 1, erlang:element(3, Trainer)}).
  ```

  ([Louis Pilfold](https://github.com/lpil))

- The compiler now allows using bit array options to specify endianness when
  constructing or pattern matching on UTF codepoints in bit arrays.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Calculations are now allowed in the size options of bit array patterns. For
  example, the following code is now valid:

  ```gleam
  let assert <<size, data:bytes-size(size / 8 - 1)>> = some_bit_array
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- On the Erlang target each generated module enables inlining from the Erlang
  compiler.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The code generated for floating point division on the JavaScript target has
  been improved to avoid performing needless checks.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The code generated for division and modulo operators on the JavaScript target
  has been improved to avoid performing needless checks.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

- `gleam update`, `gleam deps update`, and `gleam deps download` will now print
  a message when there are new major versions of packages available. For
  example:

  ```text
   $ gleam update
    Resolving versions

  The following dependencies have new major versions available:

  gleam_http 1.7.0 -> 4.0.0
  gleam_json 1.0.1 -> 3.0.1
  lustre     3.1.4 -> 5.1.1
  ```

  ([Amjad Mohamed](https://github.com/andho))

- The documentation generator now strips trailing slashes from Gitea/Forgejo
  hosts so sidebar "Repository" and "View Source" links never include `//`, and
  single-line "View Source" anchors emit `#Lx` instead of `#Lx-x`.
  ([Aayush Tripathi](https://github.com/aayush-tripathi))

- The build tool can now compile packages that will have already booted the
  Erlang compiler application instead of failing.
  ([Louis Pilfold](https://github.com/lpil))

- `gleam deps list` now uses a tab rather than a space as a separator.
  ([Louis Pilfold](https://github.com/lpil))

- The build tool now also supports `.cjs` files placed in the `src`, `dev` or
  `test` directories.
  ([yoshi](https://github.com/yoshi-monster))

- The build tool now produces better error messages when version resolution
  fails. For example:

  ```
  $ gleam add wisp@1
  Resolving versions
  error: Dependency resolution failed

  There's no compatible version of `gleam_otp`:
    - You require wisp >= 1.0.0 and < 2.0.0
      - wisp requires mist >= 1.2.0 and < 5.0.0
      - mist requires gleam_otp >= 0.9.0 and < 1.0.0
    - You require lustre >= 5.2.1 and < 6.0.0
      - lustre requires gleam_otp >= 1.0.0 and < 2.0.0

  There's no compatible version of `gleam_json`:
    - You require wisp >= 1.0.0 and < 2.0.0
      - wisp requires gleam_json >= 3.0.0 and < 4.0.0
    - You require gleam_json >= 2.3.0 and < 3.0.0
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The `repository` section in `gleam.toml` now allows specifying the
  `tag-prefix` property, which is prepended to the default tag.
  This makes it possible to have multiple packages with different versions in
  the same repository (together with `path`), without breaking links to source
  code in documentation.
  ([Sakari Bergen](https://github.com/sbergen))

### Language server

- It is now possible to use the "Pattern match on variable" code action on
  variables on the left hand side of a `use`. For example:

  ```gleam
  pub type User {
    User(id: Int, name: String)
  }

  pub fn main() {
    use user <- result.try(load_user())
    //  ^^^^ Triggering the code action here
    todo
  }
  ```

  Would result in the following code:

  ```gleam
  pub type User {
    User(id: Int, name: String)
  }

  pub fn main() {
    use user <- result.try(load_user())
    let User(id:, name:) = user
    todo
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The "generate function" and "generate variant" code actions are now
  quickfixes, allowing them to be more easily applied to code which is producing
  an error.
  ([Surya Rose](https://github.com/GearsDatapacks))

- The language server now offers a code action to remove needless blocks
  wrapping a single expression. For example, in this code snippet:

  ```gleam
  case greeting {
    User(name:) -> { "Hello, " <> name }
    //             ^^^^^^^^^^^^^^^^^^^^^ Triggering the code action
    //                                   with the cursor over this block.
    Anonymous -> "Hello, stranger!"
  }
  ```

  Would be turned into:

  ```gleam
  case greeting {
    User(name:) -> "Hello, " <> name
    Anonymous -> "Hello, stranger!"
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- It is now possible to trigger the "Add type annotation" code action anywhere
  between the start of a function definition and the start of its body. For
  example the action can trigger here while it previously wouldn't:

  ```gleam
  pub fn my_lucky_number() {
    //                    ^^ The action can trigger here as well!
    11
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

- The formatter now allows more control over how lists are split. By adding a
  trailing comma at the end of a list that can fit on a single line, the list
  will be split on multiple lines:

  ```gleam
  pub fn my_favourite_pokemon() -> List(String) {
    ["natu", "chimecho", "milotic",]
  }
  ```

  Will be formatted as:

  ```gleam
  pub fn my_favourite_pokemon() -> List(String) {
    [
      "natu",
      "chimecho",
      "milotic",
    ]
  }
  ```

  By removing the trailing comma, the formatter will try and fit the list on a
  single line again:

  ```gleam
  pub fn my_favourite_pokemon() -> List(String) {
    [
      "natu",
      "chimecho",
      "milotic"
    ]
  }
  ```

  Will be formatted back to a single line:

  ```gleam
  pub fn my_favourite_pokemon() -> List(String) {
    ["natu", "chimecho", "milotic"]
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The formatter now allows more control over how lists are formatted.
  If a list is split with multiple elements on the same line, removing the
  trailing comma will make sure the formatter keeps each item on its own line:

  ```gleam
  pub fn my_favourite_pokemon() -> List(String) {
    [
      "This list was formatted", "keeping multiple elements on the same line",
      "notice how the formatting changes by removing the trailing comma ->"
    ]
  }
  ```

  Is formatted as:

  ```gleam
  pub fn my_favourite_pokemon() -> List(String) {
    [
      "This list was formatted",
      "keeping multiple elements on the same line",
      "notice how the formatting changes by removing the trailing comma ->",
    ]
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The formatter no longer removes empty lines between list items. In case an
  empty line is added between list items they will all be split on multiple
  lines. For example:

  ```gleam
  pub fn main() {
    [
      "natu", "xatu",

      "chimeco"
    ]
  }
  ```

  Is formatted as:

  ```gleam
  pub fn main() {
    [
      "natu",
      "xatu",

      "chimeco",
    ]
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug fixes

- Fixed a bug where the language server would not show type-related code actions
  for record fields in custom type definitions.
  ([cysabi](https://github.com/cysabi))

- Fixed a bug where the "Inline variable" code action would be offered for
  function parameters and other invalid cases.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the "Inline variable" code action would not be applied
  correctly to variables using label shorthand syntax.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the compiler would emit the same error twice for patterns
  with the wrong number of labels.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would generate invalid code when the
  "Extract variable" code action was used on a `use` expression.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the compiler would crash when using the `utf8_codepoint`
  bit array segment on the JavaScript target.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where `==` and `!=` would return incorrect output for some
  JavaScript objects.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where specific combinations of options in bit array segments would
  not be allowed on the JavaScript target.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where invalid code would be generated for `let assert` in some
  cases on the JavaScript target.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where using the prelude `Ok` and `Error` values in a qualified
  fashion could cause a conflict with user-defined `Ok` and `Error` values when
  generating code on the JavaScript target.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the "Import module" code action would suggest importing
  internal modules from other packages.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the language server would not rename variables defined in
  alternative patterns.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where trying to rename a type or value from the Gleam prelude
  would result in invalid code.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the generated documentation would not be formatted properly.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server wouldn't allow you to jump to the
  definition of a record from a record update expression.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would allow using the "extract variable"
  code action on variables used in record updates.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where a record pattern with a spread `..` would not be formatted
  properly.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where fields of custom types named `prototype` would not be
  properly escaped on the JavaScript target.
  ([Surya Rose](https://github.com/GearsDatapacks))

## v1.11.1 - 2025-06-05

### Compiler

- The displaying of internal types in HTML documentation has been improved.
  ([Louis Pilfold](https://github.com/lpil))

- A warning is now emitted when the same module is imported
  multiple times into the same module with different aliases.
  ([Louis Pilfold](https://github.com/lpil))

### Bug fixes

- Fixed a bug where a bit array segment matching on a floating point number
  would match with `NaN` or `Infinity` on the JavaScript target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
