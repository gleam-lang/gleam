# Changelog

## Unreleased

## Bug Fixes

- Fixed a bug where the "Convert to case" code action would silently fail for
  every inexhaustive `let` assignment in a module other than the first one.
  ([John Downey](https://github.com/jtdowney))

## v1.17.0-rc1 - 2026-05-23

### Compiler

- The compiler now suggest public values from imported modules when the variable
  is unknown. These values are suggested based on name and arity.

  Considering this program:

  ```gleam
  import gleam/io

  pub fn main() -> Nil {
    println("Hello, World!")
  }
  ```

  The compiler will display this error message:

  ```text
    error: Unknown variable
    ┌─ /path/to/project/src/project.gleam:4:3
    │
  4 │   println("Hello, World!")
    │   ^^^^^^^

  The name `println` is not in scope here.
  Did you mean one of these:

      - io.println
  ```

  ([raphrous](https://github.com/realraphrous))

- The inference of record update expressions is now more fault tolerant: if
  there's an error in the record being updated, the compiler can still able to
  analyse the fields that are being provided.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The inference of clause guards is now more fault tolerant: if there's an error
  in a part of the guard expression, the compiler can still able to analyse the
  rest of the guard rather than stopping at the first error.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- It is now possible to use the `todo` keyword in constants, this will result in
  an helpful error message rather than a syntax error.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now prints correctly qualified or aliased type names when
  printing warnings. For example:

  ```gleam
  import user

  pub fn main() {
    user.to_string(todo)
    |> io.println
  }
  ```

  Will produce the following warning:

  ```
  warning: Todo found
    ┌─ /src/warning/wrn.gleam:4:19
    │
  4 │     user.to_string(todo)
    │                    ^^^^ This code is incomplete

  This code will crash if it is run. Be sure to finish it before
  running your program.

  Hint: I think its type is `user.User`.
  ```

  Notice how the type hint is correctly qualified for the module the warning is
  raised in.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- When writing a constant record with an empty arguments list the compiler will
  no longer stop to analyse the entire module.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now normalizes remaining-bytes bit-array checks for the
  JavaScript backend so `(bitSize - c) % 8 === 0` becomes `bitSize % 8 === 0`
  when the constant offset `c` is congruent modulo 8. This produces more uniform
  generated code for byte-aligned patterns.
  ([Daniele Scaratti](https://github.com/lupodevelop))

- The code generated for destructuring exhaustive patterns with `let` is now
  less verbose on the JavaScript target.

  ([Gavin Morrow](https://github.com/gavinmorrow))

### Build tool

- The `gleam dev` command now accepts the `--no-print-progress` flag. When this
  flag is passed, no progress information is printed.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Tables in the generated docs now look better on smaller screens.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The comment in `manifest.toml` now instructs the user to include it in their
  source control repository.
  ([Louis Pilfold](https://github.com/lpil))

- The `gleam deps outdated` command now always prints a summary showing how many
  packages have newer versions available. For example:

  ```txt
  $ gleam deps outdated
  1 of 12 packages have newer versions available.

  Package       Current  Latest
  -------       -------  ------
  gleam_stdlib  0.70.0   0.71.0
  ```

  When no packages are outdated, only the summary line is printed:

  ```txt
  $ gleam deps outdated
  0 of 12 packages have newer versions available.
  ```

  ([Daniele Scaratti](https://github.com/lupodevelop))

- The package manager now has a specific error and automatic re-authentication
  flow for when a Hex session has been revoked or has expired.
  ([Sahil Upasane](https://github.com/404salad))

- New packages are created requesting Erlang/OTP version 29 on GitHub actions.
  ([Louis Pilfold](https://github.com/lpil))

- `gleam publish` will now better discover Git repository in monorepos. This
  improves suggestions to push a tag, if it doesn't exist.
  ([Andrey Kozhev](https://github.com/ankddev))

- The `gleam export escript` command has been added for the creation of
  [escripts](https://www.erlang.org/doc/apps/erts/escript_cmd.html), BEAM
  programs bundled into a single file.
  ([Louis Pilfold](https://github.com/lpil))

### Language server

- The language server now offers a "Fill labels" code action on constants to
  automatically fill in the missing labelled arguments from a record
  constructor. For example:

  ```gleam
  pub type Pokemon {
    Pokemon(number: Int, name: String, hp: Int)
  }

  pub const cleffa = Pokemon(number: 173)
  ```

  In this code snippet we haven't specified the `name` and `hp` fields, that's
  an error! Triggering the "Fill labels" code action will result in the
  following:

  ```gleam
  pub const cleffa = Pokemon(number: 173, name: todo, hp: todo)
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- When hovering a record update expression, the language server can now show the
  fields that are not being updated. For example:

  ```gleam
  pub type Person {
    Person(name: String, age: Int)
  }

  pub fn happy_birthday_mom() {
    let mom = Person(name: "Antonella", age: 60)
    Person(..mom, age: 61)
    //     ^^^^^ Hovering this will show:
    //           Unchanged fields:
    //           - name
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server can now help with completions when typing a list's tail:

  ```gleam
  pub fn main() {
    let things_i_like = ["Gleam", "Ice Cream"]
    ["Dogs", ..t|]
    //          ^ Can now suggest a completion for `things_i_like`
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server can now help with completions when typing a record update:

  ```gleam
  pub type User {
    User(name: String, likes: List(String))
  }

  pub fn set_name(user: User, name: String) -> User {
    User(..u|)
    //      ^ Can now suggest a completion for `user`
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now has a code action to remove a redundant record update.
  For example:

  ```gleam
  pub type User {
    User(name: String, likes: List(String))
  }

  pub fn main() {
    let lucy = User(name: "Lucy", likes: ["Gleam", "Ice Cream"])
    let jak = User(..lucy, name: "Jak", likes: ["Gleam", "Dogs"])
    //             ^^^^^^ This record update is not needed!
  }
  ```

  This record update is not actually needed and will raise a warning, all fields
  are already specified. Triggering the code action anywhere on the expression
  will remove the unnecessary update:

  ```gleam
  pub type User {
    User(name: String, likes: List(String))
  }

  pub fn main() {
    let lucy = User(name: "Lucy", likes: ["Gleam", "Ice Cream"])
    let jak = User(name: "Jak", likes: ["Gleam", "Dogs"])
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- When using the wrong operator in a guard, the language server can now suggest
  and apply an automatic fix. For example:

  ```gleam
  pub fn categorise() {
    case pokemon {
      Pokemon(name:, ..) if name == "rai" + "chu" -> todo
      _ -> todo
    }
  }
  ```

  Gleam has no operator overloading, and the operator used to join strings is
  `<>`, not `+`. The language server can automatically fix this common mistake.
  Triggering the code action on the guard will replace `+` with `<>`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now presents quick fix code actions before refactoring
  ones.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now allows to further pattern match on a discard by
  replacing it with the patterns it is discarding.
  For example:

  ```gleam
  pub fn list_names(x: Result(List(String), Nil)) {
    case x {
      Error(Nil) -> io.println("no names")
      Ok(_) -> todo
      // ^ Triggering the code action here
    }
  }
  ```

  Triggering the code action will result in the following code:

  ```gleam
  pub fn list_names(x: Result(List(String), Nil)) {
    case x {
      Error(Nil) -> io.println("no names")
      Ok([]) -> todo
      Ok([first, ..rest]) -> todo
    }
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The "Generate variant" code action now automatically adds an import to use the
  generated variant if it is generated in a module from the different one.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server no longer shows completions for deprecated values from
  dependencies.
  ([Andrey Kozhev](https://github.com/ankddev))

- The language server now offers a code action to create unknown modules
  when an import is added for a module that doesn't exist.
  For example, if `import wobble/woo` is added to `src/wiggle.gleam`,
  then a code action to create `src/wobble/woo.gleam` will be presented
  when triggered over `import wobble/woo`.
  ([Cory Forsstrom](https://github.com/tarkah))

- The language server now supports finding references when triggered on an
  aliased import. For example, in the following snippet, moving the cursor
  over `log` and triggering "find references" will show all references of
  `io.println()`.

  ```gleam
  import gleam/io.{println as log}
  fn main() {
    log("Hello, world!")
  //^^^ trigger here
  }
  ```

  ([Gavin Morrow](https://github.com/gavinmorrow))

- The language server now supports `textDocument/documentHighlight` anywhere
  that `textDocument/references` is available.

  For example, triggering it with the cursor over any instance of `vec` will
  result in all of the instances of it being highlighted.

  ```gleam
  fn to_cartesian(vec) {
  //              ^^^
    let x = vec.rho * cos(vec.theta)
    //      ^^^           ^^^
    let y = vec.rho * sin(vec.theta)
    //      ^^^           ^^^
    #(x, y)
  }
  ```

  ([Gavin Morrow](https://github.com/gavinmorrow))

### Formatter

### Releases

- A `gleam-licences.html` is now included with each release, detailing the
  licences of the used dependencies.
  ([Louis Pilfold](https://github.com/lpil))

### Bug fixes

- Fixed a bug where `gleam remove` would fail with a confusing File IO error
  if `manifest.toml` didn't exist yet (e.g. in a freshly-created project or
  after the manifest had been deleted).
  ([Charlie Tonneslan](https://github.com/c-tonneslan))

- Fixed a bug where the build tool would check for new major versions of a local
  or git dependency on Hex when running `gleam update`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the "pattern match on value" code action would generate
  invalid code when used on a `let` assignment on the right hand side of another
  `let` assignment.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler wouldn't track the minimum required version
  when using list prepending in constants.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server wouldn't let one extract record
  constructors as variables.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would suggest completions for values
  from the language's prelude, even though their types were incompatible with
  the current context.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would suggest the "wrap in anonymous"
  code action even when not hovering directly over a function.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would suggest the "wrap in anonymous"
  code action when hovering over a record update.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would generate invalid code for guards using
  lists with a tail.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would suggest the "convert to case" code
  action even when not explicitly hovering an inexhaustive let assignment.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would suggest the "add missing pattern"
  code action even when not explicitly hovering an inexhaustive case expression.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would suggest the "unqualify" code
  action even when not explicitly hovering a qualified value.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would suggest the "qualify" code
  action even when not explicitly hovering an unqualified type or constructor.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would suggest the "generate dynamic
  decoder" code action even when not explicitly hovering a custom type.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would suggest the "generate json
  encoder" code action even when not explicitly hovering a custom type.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would suggest the "missing type
  parameter" code action even when not explicitly hovering a custom type.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would suggest the "unwrap anonymous
  function" code action even when not explicitly hovering a custom type.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would suggest the "extract function"
  code action even when selecting multiple branches of a case expression, or
  patterns and guards of a case arm.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would not warn for ints over the safe
  JavaScript limit in `BitArray` segments with a unit option.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would incorrectly warn for ints over the
  safe JavaScript limit in `BitArray` byte segments that aren't ints.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a confusing error message when writing a constant bit array with a size
  that is not a literal number.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a confusing error message when writing bit arrays with an invalid unit.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a confusing error message when writing a constant bit array with an
  invalid segment.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a confusing error message when writing `@external` or `@deprecated`
  annotations with arguments that are not string.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where enabling `javascript.typescript_declarations` or
  `javascript.source_maps` wouldn't generate their additional files unless the
  build directory was manually deleted. The compiler now automatically rebuilds
  the project when this configuration changes.
  ([daniellionel01](https://github.com/daniellionel01))

- Fixed a bug where using the `bytes` and `unit` options together on a bit array
  segment could generate incorrect code on the JavaScript target.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the JavaScript code generator could produce duplicate `let`
  declarations for internal variables after a `case` expression whose subject
  directly matches branch when existing variables with same exist in outer
  scope.
  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where `gleam publish` wrote the wrong application name into the
  published package metadata for dependencies whose Hex package name differs
  from their internal OTP application name. This caused Mix-based projects to
  fail to build when they depended on Gleam packages with transitive
  dependencies.
  ([Logan Bresnahan](https://github.com/LoganBresnahan))

- Fixed a bug where cli would fail to complete https connections from behind a
  proxy with self-signed certificates. The cli now defaults to using system
  trust stores for trusted CAs, allowing use in proxied network environments.
  ([apsoras](https://github.com/apsoras))

- Fixed a bug where the language server's "add missing patterns" code action
  would not be offered when the cursor was on an inexhaustive `case` expression
  if another inexhaustive `case` appeared earlier in the same module.
  ([John Downey](https://github.com/jtdowney))
