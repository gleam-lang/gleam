# Changelog

## Unreleased

### Compiler

- Improved code generation for blocks in tail position on the Javascript target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Function documentation comments and module documentation comments are now
  included in the generated Erlang code and can be browsed from the Erlang
  shell starting from OTP27.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

### Language server

- The language server can now fill in the labels of any function call, even when
  only some of the arguments are provided. For example:

  ```gleam
  import gleam/string

  pub fn main() {
    string.replace("wibble")
  }
  ```

  Will be completed to:

  ```gleam
  import gleam/string

  pub fn main() {
    string.replace("wibble", each: todo, with: todo)
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now suggests a code action to pattern match on a
  function's argument. For example:

  ```gleam
  pub type Pokemon {
    Pokemon(pokedex_number: Int, name: String)
  }

  pub fn to_string(pokemon: Pokemon) {
    //             ^ If you put your cursor over the argument
    todo
  }
  ```

  Triggering the code action on the `pokemon` argument will generate the
  following code for you:

  ```gleam
  pub type Pokemon {
    Pokemon(pokedex_number: Int, name: String)
  }

  pub fn to_string(pokemon: Pokemon) {
    let Pokemon(pokedex_number:, name:) = pokemon
    todo
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

### Bug fixes

- Fixed a bug where the "convert from use" code action would generate invalid
  code for use expressions ending with a trailing comma.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where build tool could fail to add new dependencies when
  dependencies with optional dependencies are present in the manifest.
  ([Louis Pilfold](https://github.com/lpil))
