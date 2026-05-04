# Changelog

## Unreleased

### Compiler

- The inference of record update expressions is now more fault tolerant: if
  there's an error in the record being updated, the compiler can still able to
  analyse the fields that are being provided.
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

### Build tool

- The `gleam dev` command now accepts the `--no-print-progress` flag. When this
  flag is passed, no progress information is printed.
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

### Language server

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

### Formatter

### Bug fixes

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
