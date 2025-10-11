# Changelog

## v1.13.0-rc2 - 2025-10-06

### Bug fixes

- Fixed a bug where the "Extract function" code action would not properly
  extract a `use` expression.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the "Extract function" code action would generate a function
  with the wrong type when used on a use expression.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the error message for inexhaustive patterns could show
  incorrect extra patterns in addition to the correct missing patterns.
  ([Adi Salimgereyev](https://github.com/abs0luty))

- Fixed a bug where triggering the "Generate function" code action to generate
  a function in a different module could cause the generated function to appear
  in the middle of an existing function, resulting in invalid code.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the "turn into pipe" code action would not trigger inside
  the final step of a pipeline.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the "pattern match on variable" code action would crash when
  used on a variable followed by a case expression.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.13.0-rc1 - 2025-09-29

### Compiler

- The compiler now applies an optimisation known as "interference based pruning"
  when compiling bit array pattern matching where matches are performed at the
  start of bit arrays.
  This optimisation drastically reduces compile times, memory usage and the
  compiled code size, removing many redundant checks.
  It is particularly important for network protocol applications where it is
  typical to match on some fixed patterns at the start of the bitarray.
  For example:

  ```gleam
  pub fn parser_headers(headers: BitArray, bytes: Int) -> Headers {
    case headers {
      <<"CONTENT_LENGTH" as header, 0, value:size(bytes), 0, rest:bytes>>
      | <<"QUERY_STRING" as header, 0, value:size(bytes), 0, rest:bytes>>
      | <<"REQUEST_URI" as header, 0, value:size(bytes), 0, rest:bytes>>
      // ...
      | <<"REDIRECT_STATUS" as header, 0, value:size(bytes), 0, rest:bytes>>
      | <<"SCRIPT_NAME" as header, 0, value:size(bytes), 0, rest:bytes>>
        -> [#(header, value), ..parse_headers(rest)]
    }
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now raises a warning for unreachable branches that are matching
  on bit array segments that could never match. Consider this example:

  ```gleam
  pub fn get_payload(packet: BitArray) -> Result(BitArray, Nil) {
    case packet {
      <<200, payload:bytes>> -> Ok(payload)
      <<404, _:bits>> -> Error(Nil)
      _ -> Ok(packet)
    }
  }
  ```

  There's a subtle bug here. The second branch can never match since it's
  impossible for the first byte of the bit array to have the value `404`.
  The new error explains this nicely:

  ```text
  warning: Unreachable pattern
    ┌─ /src.gleam:4:5
    │
  4 │     <<404, _:bits>> -> Error(Nil)
    │     ^^^^^^^^^^^^^^^
    │       │
    │       A 1 byte unsigned integer will never match this value

  This pattern cannot be reached as it contains segments that will never
  match.

  Hint: It can be safely removed.
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now raises a warning when a function's argument is only passed
  along in a recursive call but not actually used for anything. For example:

  ```gleam
  import gleam/io

  pub fn greet(x, times) {
    case times {
      0 -> Nil
      _ -> {
        io.println("Hello, Joe!")
        greet(x, times - 1)
      }
    }
  }
  ```

  In this piece of code the `x` argument is actually never used, and the
  compiler will raise the following warning:

  ```text
  warning: Unused function argument
    ┌─ /Users/giacomocavalieri/Desktop/prova/src/prova.gleam:3:14
    │
  3 │ pub fn greet(x, times) {
    │              ^ This argument is unused

  This argument is passed to the function when recursing, but it's never used
  for anything.
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now emits a better error message for private types marked as
  opaque. For example, the following piece of code:

  ```gleam
  opaque type Wibble {
    Wobble
  }
  ```

  Would result in the following error:

  ```
  error: Private opaque type
    ┌─ /src/one/two.gleam:2:1
    │
  2 │ opaque type Wibble {
    │ ^^^^^^ You can safely remove this.

  Only a public type can be opaque.
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The parsing of opaque private types is now fault tolerant: having a private
  opaque type in a module no longer stops the compiler from highlighting other
  errors.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now emits a single warning for multiple negations in a row, while
  previously it would emit multiple comments highlighting increasingly longer
  spans.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Redundant `_ as x` patterns are now deprecated in favour of `x`.
  ([eutampieri](https://github.com/eutampieri))

- The compiler will now raise warning for inefficient use of `list.length()`
  when trying to check is list empty via `0 < list.length(list)` or
  `list.length(list) > 0` as well as in other cases. For example, the following
  code:

  ```gleam
  import gleam/list

  pub fn main() {
    let numbers = [1, 46]
    let _ = 0 < list.length(numbers)
    let _ = list.length(numbers) > 0
  }
  ```

  Would result in following warnings:

  ```
  warning: Inefficient use of `list.length`
    ┌─ /data/data/com.termux/files/home/test_gleam/src/test_gleam.gleam:5:13
    │
  5 │     let _ = 0 < list.length(numbers)
    │             ^^^^^^^^^^^^^^^^^^^^^^^^

  The `list.length` function has to iterate across the whole
  list to calculate the length, which is wasteful if you only
  need to know if the list is empty or not.

  Hint: You can use `the_list != []` instead.

  warning: Inefficient use of `list.length`
    ┌─ /data/data/com.termux/files/home/test_gleam/src/test_gleam.gleam:6:13
    │
  6 │     let _ = list.length(numbers) > 0
    │             ^^^^^^^^^^^^^^^^^^^^^^^^

  The `list.length` function has to iterate across the whole
  list to calculate the length, which is wasteful if you only
  need to know if the list is empty or not.

  Hint: You can use `the_list != []` instead.
  ```

  ([Andrey Kozhev](https://github.com/ankddev))

- The compiler now provides an improved error message for when trying to define
  a constant inside a function. For example, the following code:

  ```gleam
  pub fn deep_thought() {
    const the_answer = 42
    the_answer
  }
  ```

  Will produce this error message:

  ```txt
    error: Syntax error
    ┌─ /src/file.gleam:2:3
    │
  3 │   const the_answer = 43
    │   ^^^^^ Constants are not allowed inside functions

  All variables are immutable in Gleam, so constants inside functions are not
  necessary.
  Hint: Either move this into the global scope or use `let` binding instead.
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- The code generated for blocks on the JavaScript target has been improved and
  is now smaller in certain cases.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Writing a type name followed by `()` now emits an error during analysis
  rather than parsing, so it no longer stops the compiler from reporting errors
  further in the code.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now shows a specific syntax error when trying to use an
  angle-bracket syntax for generic types or function definitions:

  ```txt
  error: Syntax error
    ┌─ /src/parse/error.gleam:2:12
    │
  2 │ type Either<a, b> {
    │            ^ I was expecting `(` here.

  Type parameters use lowercase names and are surrounded by parentheses.

      type Either(a, b) {

  See: https://tour.gleam.run/data-types/generic-custom-types/
  ```

  ([Aaron Christiansen](https://github.com/AaronC81))

- Fault tolerance for analysis of labeled fields in constructor patterns has
  been improved.
  ([sobolevn](https://github.com/sobolevn))

- Compiler now adds a hint when `#`-styled comments are used. This code:

  ```gleam
  fn some() {
    let a = 1
    # let b = 2
  }
  ```

  Now produces:

  ```txt
  error: Syntax error
    ┌─ /src/main.gleam:3:5
    │
  3 │   # let b = 2
    │     ^^^ I was not expecting this

  Found the keyword `let`, expected one of:
  - `(`
  Hint: Maybe you meant to create a comment?
  Comments in Gleam start with `//`, not `#`
  ```

  ([sobolevn](https://github.com/sobolevn))

- The `erlang.application_start_argument` parameter has been added to
  `gleam.toml`. This is a string containing an Erlang term that will be written
  into the package's Erlang `.app` file if `erlang.application_start_module`
  has been set, replacing the default argument of `[]`.
  ([Louis Pilfold](https://github.com/lpil))

- Generated code for the JavaScript target now includes a public API which can
  be used for FFI interacting with Gleam custom types. For example, if you have
  this Gleam code:

  ```gleam
  pub type Person {
    Teacher(name: String, subject: String)
    Student(name: String, age: Int)
  }
  ```

  You can use the new API to use the `Person` type in FFI code:

  ```javascript
  import * from "./person.mjs";

  // Constructing custom types
  let teacher = Person$Teacher("Joe Armstrong", "Computer Science");
  let student = Person$Student("Louis Pilfold", 17);

  let randomPerson = Math.random() > 0.5 ? teacher : student;

  // Checking variants
  let randomIsTeacher = Person$isTeacher(randomPerson);

  // Getting fields
  let studentAge = Person$Student$age(student);

  // The `name` field is shared so can be accessed from either variant
  let personNAme = Person$name(randomPerson);
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

### Build tool

- New projects are generated using OTP28 on GitHub Actions.
  ([Louis Pilfold](https://github.com/lpil))

- The build tool now has a new `hex owner transfer` subcommand to transfer
  ownership of existing packages.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- `gleam add` now adds `dependencies` and `dev-dependencies` as tables instead
  of inline tables if they are missing.
  ([Andrey Kozhev](https://github.com/ankddev))

- After dependency resolution the build tool will now print all packages added
  and removed, and any versions changed.
  ([Louis Pilfold](https://github.com/lpil))

- `gleam publish` now blocks publishing packages that contain the default main
  function to prevent accidental publishing of unmodified template code.
  ([Joohoon Cha](https://github.com/jcha0713))

- When generating documentation, the build tool will now print the names of
  public type aliases instead of internal type names when annotating functions
  and types. For example, for the following code:

  ```gleam
  import my_package/internal

  pub type ExternalAlias = internal.InternalRepresentation

  pub fn do_thing() -> ExternalAlias { ... }
  ```

  This is what the build tool used to generate:

  ```gleam
  pub fn do_thing() -> @internal InternalRepresentation
  ```

  Whereas now it will not use the internal name, and instead produce:

  ```gleam
  pub fn do_thing() -> ExternalAlias
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- Support has been added for using Tangled as a repository.
  ([Naomi Roberts](https://github.com/naomieow))

### Language server

- The language server now offers a code action to remove all the unreachable
  branches in a case expression. For example:

  ```gleam
  pub fn main() {
    case find_user() {
      Ok(user) -> todo
      Ok(Admin) -> todo
  //  ^^^^^^^^^ This branch is unreachable
      Ok(User) -> todo
  //  ^^^^^^^^ This branch is unreachable
      Error(_) -> todo
    }
  }
  ```

  Hovering over one of the unreachable branches and triggering the code action
  would remove all the unreachable branches:

  ```gleam
  pub fn main() {
    case find_user() {
      Ok(user) -> todo

      Error(_) -> todo
    }
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The "pattern match on variable" can now be triggered on lists. For example:

  ```gleam
  pub fn is_empty(list: List(a)) -> Bool {
    //            ^^^^ Triggering the action over here
  }
  ```

  Triggering the action over the `list` argument would result in the following
  code:

  ```gleam
  pub fn is_empty(list: List(a)) -> Bool {
    case list {
      [] -> todo
      [first, ..rest] -> todo
    }
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The "pattern match on variable" code action can now be triggered on variables
  introduced by other patterns. For example:

  ```gleam
  pub fn main() {
    let User(name:, role:) = find_user("lucy")
    //              ^^^^ Triggering the action over
  }
  ```

  Triggering the action over another variable like `role` would result in the
  following code:

  ```gleam
  pub fn main() {
    let User(name:, role:) = find_user("lucy")
    case role {
      Admin -> todo
      Member -> todo
    }
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The "pattern match on variable" code action can now be triggered on variables
  in case expressions. For example:

  ```gleam
  pub fn main() {
    case find_user() {
      Ok(user) -> todo
      Error(_) -> todo
    }
  }
  ```

  Triggering the action over the `user` variable would result in the following
  code:

  ```gleam
  pub fn main() {
    case find_user() {
      Ok(Admin) -> todo
      Ok(Member) -> todo
      Error(_) -> todo
    }
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now offers a quick fix to remove `opaque` from a private
  type:

  ```gleam
  opaque type Wibble {
  // ^^^ This is an error!
    Wobble
  }
  ```

  If you hover over the type and trigger the quick fix, the language server will
  automatically remove the `opaque` keyword:

  ```gleam
  type Wibble {
    Wobble
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now offers a code action to add the omitted labels in a
  call. For example:

  ```gleam
  pub type User {
    User(first_name: String, last_name: String, likes: List(String))
  }

  pub fn main() {
    let first_name = "Giacomo"
    User(first_name, "Cavalieri", ["gleam"])
  //^^^^ Triggering the code action over here
  }
  ```

  Triggering the code action over the `User` constructor will result in the
  following code:

  ```gleam
  pub type User {
    User(first_name: String, last_name: String, likes: List(String))
  }

  pub fn main() {
    let first_name = "Giacomo"
    User(first_name:, last_name: "Cavalieri", likes: ["gleam"])
  }
  ```

- The "inline variable" code action is now only suggested when hovering over the
  relevant variable.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- When hovering over a record field in a record access expression, the language
  sever will now show the documentation for that field, if present.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Renaming a variable from a label shorthand (`name:`) no longer includes the
  colon in the rename dialog (`name:` -> `name`)
  ([fruno](https://github.com/frunobulax-the-poodle))

- The language server now offers a code action to collapse nested case
  expressions. Take this example:

  ```gleam
  case user {
    User(role: Admin, name:) ->
      // Here the only thing we're doing is pattern matching on the
      // `name` variable we've just defined in the outer pattern.
      case name {
        "Joe" -> "Hello, Joe!"
        _ -> "Hello, stranger"
      }

    _ -> "You're not an admin!"
  }
  ```

  We could simplify this case expression and reduce nesting like so:

  ```gleam
  case user {
    User(role: Admin, name: "Joe") -> "Hello, Joe!"
    User(role: Admin, name: _) -> "Hello, stranger"
    _ -> "You're not an admin!"
  }
  ```

  Now, if you hover over that pattern, the language server will offer the
  "collapse nested case" action that will simplify your code like shown in the
  example above.

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The "Generate function" code action now allows generating function in other
  modules. For example, given the following code:

  ```gleam
  // maths.gleam
  pub fn add(a: Int, b: Int) -> Int { a + b }

  // main.gleam
  import maths

  pub fn main() -> Int {
    echo maths.add(1, 2)
    echo maths.subtract(from: 2, subtract: 1)
    //         ^ Trigger the "Generate function" code action here
  }
  ```

  The language sever will edit the `maths.gleam` file:

  ```gleam
  pub fn add(a: Int, b: Int) -> Int { a + b }

  pub fn subtract(from from: Int, subtract subtract: Int) -> Int {
    todo
  }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- The "Add type annotations" and "Generate function" code actions now ignore
  type variables defined in other functions, improving the generated code.
  For example:

  ```gleam
  fn something(a: a, b: b, c: c) -> d { todo }

  fn pair(a, b) { #(a, b) }
  ```

  Previously, when triggering the "Add type annotations" code action on the
  `pair` function, the language server would have generated:

  ```gleam
  fn pair(a: e, b: f) -> #(e, f) { #(a, b) }
  ```

  However in 1.13, it will now generate:

  ```gleam
  fn pair(a: a, b: b) -> #(a, b) { #(a, b) }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- You can now go to definition, rename, etc. from alternative patterns!

  ```gleam
  case wibble {
    Wibble | Wobble -> 0
    //         ^- Previously you could not trigger actions from here
  }

  ```

  ([fruno](https://github.com/fruno-bulax))

- When showing types of values on hover, or adding type annotations, the language
  server will now prefer public type aliases to internal types. For example, if
  the "Add type annotations" code action was triggered on the following code:

  ```gleam
  import lustre/html
  import lustre/element
  import lustre/attribute

  pub fn make_link(attribute, element) {
    html.a([attribute], [elements])
  }
  ```

  Previously, the following code would have been generated:

  ```gleam
  pub fn make_link(
    attribute: vattr.Attribute,
    element: vdom.Element(a)
  ) -> vdom.Element(a) {
     html.a([attribute], [elements])
  }
  ```

  Which references internal types which should not be imported by the user.
  However, now the language server will produce the following:

  ```gleam
  pub fn make_link(
    attribute: attribute.Attribute,
    element: element.Element(a)
  ) -> element.Element(a) {
     html.a([attribute], [elements])
  }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- The language server now offers the `convert to case` code action only if a
  single `let assert` expression is selected.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now offers an "Extract function" code action to extract a
  selected piece of code into a separate function. For example:

  ```gleam
  const head_byte_count = 256

  pub fn get_head_of_file() {
    let assert Ok(contents) = read_file()

    case contents {
  //^ Select from here
      <<head:bytes-size(head_byte_count), _:bits>> -> Ok(head)
      _ -> Error(Nil)
    }
  //^ Until here
  }
  ```

  Would become:

  ```gleam
  const head_byte_count = 256

  pub fn get_head_of_file() {
    let assert Ok(contents) = read_file()

    function(contents)
  }

  fn function(contents: BitArray) -> Result(BitArray, Nil) {
    case contents {
      <<head:bytes-size(head_byte_count), _:bits>> -> Ok(head)
      _ -> Error(Nil)
    }
  }
  ```

  You can then use language server renaming to choose an appropriate name for
  the new function.

  ([Surya Rose](https://github.com/GearsDatapacks))

  - The function signature helper now displays original function definition
  generic names when arguments are unbound. For example, in this code:

  ```gleam
  pub fn wibble(x: something, y: fn() -> something, z: anything) { Nil }

  pub fn main() {
      wibble( )
          // ↑
  }
  ```

  will show a signature help

  ```gleam
  wibble(something, fn() -> something, anything)

  ```

  instead of

  ```gleam
  wibble(a, fn() -> a, b) -> Nil
  ```

  ([Samuel Cristobal](https://github.com/scristobal)) and
  ([Surya Rose](https://github.com/GearsDatapacks))

### Formatter

- The formatter now removes needless multiple negations that are safe to remove.
  For example, this snippet of code:

  ```gleam
  pub fn useless_negations() {
    let lucky_number = --11
    let lucy_is_a_star = !!!False
  }
  ```

  Is rewritten as:

  ```gleam
  pub fn useless_negations() {
    let lucky_number = 11
    let lucy_is_a_star = !False
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Redundant `_ as x` patterns are rewritten to `x`.
  ([eutampieri](https://github.com/eutampieri))

- The formatter no longer removes blocks from case clause guards.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The formatter now properly formats tuple return annotation with comments.
  ([Andrey Kozhev](https://github.com/ankddev))

### Bug fixes

- Fixed a bug where literals using `\u{XXXX}` syntax in bit array pattern
  segments were not translated to Erlang's `\x{XXXX}` syntax correctly.
  ([Benjamin Peinhardt](https://github.com/bcpeinhardt))

- Fixed a bug where `echo` could crash on JavaScript if the module contains
  record variants with the same name as some built-in JavaScript objects.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where the compiler would highlight an entire double negation
  expression as safe to remove, instead of just highlighting the double
  negation.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would crash if there was an invalid version
  requirement in a project's `gleam.toml`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would suggest a discouraged project name as an
  alternative to the reserved `gleam` name.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where Forgejo source URLs in the HTML documentation could be
  incorrectly structured.
  ([Louis Pilfold](https://github.com/lpil))

- The compiler now emits an error when a module in the `src` directory imports
  a dev dependency, while previously it would incorrectly let these
  dependencies to be imported.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Erroneous extra fields in `gleam.toml` dependency specifications will no
  longer be siltently ignored. An error is now returned highlighting the
  problem instead.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where renaming a constant which is referenced in another module
  inside a guard would generate invalid code.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where `echo .. as ..` message will be omitted in browser target.
  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where renaming a variable used in a record update would produce
  invalid code in certain situations.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where adding `echo` to the subject of a `case` expression would
  prevent variant inference from working correctly.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the compiler would suggest to use a discarded value defined
  in a different function.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the formatter would format a panic message adding more
  nesting than necessary.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server wouldn't offer the "unqualify" code
  action if used on a type alias.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would fail to rename an external
  function with no body.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler allowed to write a guard with an empty clause.
  ([Tristan-Mihai Radulescu](https://github.com/Courtcircuits))

- Fixed a bug where switching from a hex dependency to a git dependency would
  result in an error from the compiler.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would reference a redeclared variable in a let
  assert message, instead of the original variable, on the Erlang target.
  ([Danielle Maywood](https://github.com/DanielleMaywood))

- Fixed a bug where the compiler would report an imported module as unused if it
  were used by private functions only.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the "Extract variable" code action would shadow existing
  variables, constants and function names.
  ([Matias Carlander](https://github.com/matiascr))

- Fixed a bug where the language server would not fill in the missing labels of
  a pattern correctly, generating invalid code.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where invalid code was being generated when using the "Extract
  variable" code action inside an anonymous function.
  ([Matias Carlander](https://github.com/matiascr))

- Fixed a bug where running `gleam update` would not properly update git
  dependencies unless `gleam clean` was run first.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the compiler would produce wrong JavaScript code for binary
  pattern matching expressions using literal strings and byte segments.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the build tool would crash when trying to add transitive dependency.
  ([Andrey Kozhev](https://github.com/ankddev))
