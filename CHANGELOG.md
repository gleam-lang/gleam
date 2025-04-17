# Changelog

## Unreleased

### Compiler

- The compiler can now tell if some branches with bit array patterns are
  unreachable. For example, the following code:

  ```gleam
  case payload {
    <<first_byte, _:bits>> -> first_byte
    <<1, _:bits>> -> 1
    _ -> 0
  }
  ```

  Will raise the following warning:

  ```
  warning: Unreachable case clause
    ┌─ /src/bit_array.gleam:4:5
    │
  4 │     <<1, _:bits>> -> 1
    │     ^^^^^^^^^^^^^^^^^^
  This case clause cannot be reached as a previous clause matches the same
  values.
  Hint: It can be safely removed.
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

- The build tool now supports placing modules in a directory called `dev`,
  which like `test`, is only for development code.
  ([Surya Rose](https://github.com/GearsDatapacks))

- There is now a new CLI command, `gleam dev`, which runs the `$PACKAGE_dev`
  module, for running development entrypoints.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Language server

- The code action to add missing labels to function now also works in patterns:

  ```gleam
  pub type Person {
    Person(name: String, age: Int, job: String)
  }

  pub fn age(person: Person) {
    let Person(age:) = person
    age
  }
  ```

  Becomes:

  ```gleam
  pub type Person {
    Person(name: String, age: Int, job: String)
  }

  pub fn age(person: Person) {
    let Person(age:, name:, job:) = person
    age
  }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- The JSON encoding function that the language server code action generates is
  now named `$TYPENAME_to_json` instead of `encode_$TYPENAME`. This is to remove
  ambiguity with functions that encode to other formats, and to make the
  function easier to discover by searching.

  ([Louis Pilfold](https://github.com/lpil))

### Formatter

### Bug fixes
