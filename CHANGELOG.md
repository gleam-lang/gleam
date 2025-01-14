# Changelog

## Unreleased

### Compiler

- Pipelines are now fault tolerant. A type error in the middle of a pipeline
  won't stop the compiler from figuring out the types of the remaining pieces,
  enabling the language server to show better suggestions for incomplete pipes.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Improved code generation for blocks in tail position on the Javascript target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Function documentation comments and module documentation comments are now
  included in the generated Erlang code and can be browsed from the Erlang
  shell starting from OTP27.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Parsing of `case` expressions is now fault tolerant. If a `case` expressions
  is missing its body, the compiler can still perform type inference. This also
  allows the Language Server to provide completion hints for `case` subjects.
  ([Surya Rose](https://github.com/GearsDatapacks))

- The compiler can now suggest to wrap a value in an `Ok` or `Error` if that can
  solve a type mismatch error:

  ```gleam
  pub fn greet_logged_user() {
    use <- bool.guard(when: !logged_in, return: Error(Nil))
    "Hello!"
  }
  ```

  Results in the following error:

  ```txt
  error: Type mismatch
    ┌─ /main.gleam:7:3
    │
  7 │   "Hello!"
    │   ^^^^^^^^ Did you mean to wrap this in an `Ok`?

  Expected type:

      Result(a, Nil)

  Found type:

      String
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

- `gleam new` now has refined project name validation - rather than failing on
  invalid project names, it suggests a valid alternative and prompts for
  confirmation to use it.
  ([Diemo Gebhardt](https://github.com/diemogebhardt))

- `gleam docs build` generated documentation site now focuses the search input
  when "Cmd/Ctrl + K", "s" or "/" is pressed.
  ([Sambit Sahoo](https://github.com/soulsam480))

- `gleam deps` now supports `tree` operation that lists the dependency tree.

  ```markdown
  Usage: gleam deps tree [OPTIONS]

  Options:
    -p, --package <PACKAGE>  Package to be used as the root of the tree
    -i, --invert <PACKAGE>   Invert the tree direction and focus on the given package
    -h, --help               Print help
  ```

  ([Ramkarthik Krishnamurthy](https://github.com/ramkarthik))

### Language server

- The language server can now generate the definition of functions that do not
  exist in the current file. For example if I write the following piece of code:

  ```gleam
  import gleam/io

  pub type Pokemon {
    Pokemon(pokedex_number: Int, name: String)
  }

  pub fn main() {
    io.println(to_string(pokemon))
    //          ^ If you put your cursor over this function that is
    //            not implemented yet
  }
  ```

  Triggering the "generate function" code action, the language server will
  generate the following function for you:

  ```gleam
  fn to_string(pokemon: Pokemon) -> String {
    todo
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

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

- The language server now suggests a code action to pattern match on a variable.
  For example:

  ```gleam
  pub fn main() {
    let result = list.first(a_list)
    //  ^ If you put your cursor over the variable
    todo
  }
  ```

  Triggering the code action on the `result` variable will generate the
  following code for you:

  ```gleam
  pub fn main() {
    let result = list.first(a_list)
    case result {
      Ok(value) -> todo
      Error(value) -> todo
    }
    todo
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- When generating functions or variables, the language server can now pick
  better names using the type of the code it's generating.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Formatter

### Bug fixes

- Fixed a bug where the "convert from use" code action would generate invalid
  code for use expressions ending with a trailing comma.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where floats outside of Erlang's floating point range were not
  causing errors.
  ([shayan](https://github.com/massivefermion))

- Fixed a bug where build tool could fail to add new dependencies when
  dependencies with optional dependencies are present in the manifest.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where a block expression containing a singular record update would
  produce invalid erlang.
  ([yoshi](https://github.com/joshi-monster))

- Fixed a typo in the error message when trying to import a test module into an
  application module.
  ([John Strunk](https://github.com/jrstrunk))

- Fixed a bug where the "Extract variable" code action would erroneously extract
  a pipeline step as a variable.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
