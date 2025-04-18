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

- The compiler will now include labels in the error message when a `case`
  expression is inexhaustive. For example, this code:

  ```gleam
  pub type Person {
    Person(name: String, age: Int)
  }

  pub fn classify(person: Person) {
    case person {
      Person(name: "John", age: 27) -> todo
      Person(name: _, age: 42) -> todo
    }
  }
  ```

  Will produces this error:

  ```
  error: Inexhaustive patterns
    ┌─ /src/main.gleam:6:3
    │
  6 │ ╭   case person {
  7 │ │     Person(name: "John", age: 27) -> todo
  8 │ │     Person(name: _, age: 42) -> todo
  9 │ │   }
    │ ╰───^

  This case expression does not have a pattern for all possible values. If it
  is run on one of the values without a pattern then it will crash.

  The missing patterns are:

      Person(name:, age:)
  ```

### Build tool

- The build tool now supports placing modules in a directory called `dev`,
  which like `test`, is only for development code.
  ([Surya Rose](https://github.com/GearsDatapacks))

- There is now a new CLI command, `gleam dev`, which runs the `$PACKAGE_dev`
  module, for running development entrypoints.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Updated the Erlang shipment POSIX entrypoint script to add an exec statement
  so the Erlang process replaces the shell's process and can receive signals
  when deployed.
  ([Christopher De Vries](https://github.com/devries))

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

- The code action to add missing patterns to a `case` expression now includes
  labels in the generated patterns. For example:

  ```gleam
  pub type Person {
    Person(name: String, age: Int)
  }

  pub fn classify(person: Person) {
    case person {
      Person(name: "John", age: 27) -> todo
      Person(name: _, age: 42) -> todo
    }
  }
  ```

  Will now become:

  ```gleam
  pub type Person {
    Person(name: String, age: Int)
  }

  pub fn classify(person: Person) {
    case person {
      Person(name: "John", age: 27) -> todo
      Person(name: _, age: 42) -> todo
      Person(name:, age:) -> todo
    }
  }
  ```

### Formatter

### Bug fixes
