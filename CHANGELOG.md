# Changelog

## Unreleased

### Compiler

- The inference of record update expressions is now more fault tolerant: if
  there's an error in the record being updated, the compiler can still able to
  analyse the fields that are being provided.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

- The `gleam dev` command now accepts the `--no-print-progress` flag. When this
  flag is passed, no progress information is printed.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

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
