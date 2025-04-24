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

- The compiler now raises a warning when it can tell that an integer segment
  with a literal value is going to be truncated. For example, if you wrote this:

  ```gleam
  <<258>>
  ```

  The compiler will now warn you:

  ```txt
  warning: Truncated bit array segment
    ┌─ /src/main.gleam:4:5
    │
  4 │   <<258>>
    │     ^^^ You can safely replace this with 2

  This segment is 1 byte long, but 258 doesn't fit in that many bytes. It
  would be truncated by taking its its first byte, resulting in the value 2.
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

  ([Surya Rose](https://github.com/GearsDatapacks))

- The analysis of lists, tuples, negation operators, `panic`, `echo` and `todo`
  is now fault tolerant, meaning that the compiler will not stop reporting
  errors as soon as it finds one.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The error message for types used with the wrong number of arguments has been
  improved. For example, this piece of code:

  ```gleam
  type Wibble(a)

  type Wobble {
    Wobble(Wibble)
  }
  ```

  Produces the following error:

  ```txt
    error: Incorrect arity
    ┌─ /src/one/two.gleam:5:10
    │
  5 │   Wobble(Wibble)
    │          ^^^^^^ Expected 1 type argument, got 0

  `Wibble` requires 1 type argument but none where provided.
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- You can now use the `assert` keyword by itself to test a boolean expression.
  If the expression evaluates to `False` at runtime, the `assert` statement
  will cause the program to panic, with information about the expression that
  was asserted.

  For example:

  ```gleam
  pub fn ok_error_test() {
    assert result.is_ok(Ok(10))
    assert result.is_error(Error("Some error"))
    assert Ok(1) != Error(1)
    assert result.is_error(Ok(42)) // panic: Assertion failed
  }
  ```

  A custom panic message can also be provided in order to add extra information:

  ```gleam
  pub fn identity_test() {
    assert function.identity(True) as "Identity of True should never be False"
  }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- The compiler will now emit a warning when the return value of a call to a
  pure function is unused. For example the following code:

  ```gleam
  fn add(a, b) { a + b }

  pub fn main() {
    add(1, 2)
    Nil
  }
  ```

  Will produce the following warning:

  ```
  warning: Unused pure function call
    ┌─ /home/gears/projects/playground/gleam/src/playground.gleam:4:3
    │
  4 │   add(1, 2)
    │   ^^^^^^^^^ This pure function call is never used

  Gleam is an immutable language, meaning functions cannot mutate state
  simply by being called. This function is pure, so it must be assigned to a
  variable to have an effect on the program.
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

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

- The build tool now provides additional information when printing warnings for
  deprecated environment variables.
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

  ([Surya Rose](https://github.com/GearsDatapacks))

- The language server now provides hover, autocomplete and goto definition
  for constant definitions.
  ([Surya Rose](https://github.com/GearsDatapacks))

- The "generate function" code action can now choose better names based on the
  labels and variables used. For example if I write the following code:

  ```gleam
  pub fn main() -> List(Int) {
    let list = [1, 2, 3]
    let number = 1
    remove(each: number, in: list)
  //^^^^ This function doesn't exist yet!
  }
  ```

  And ask the language server to generate the missing function, the generated
  code will now look like this:

  ```gleam
  fn remove(each number: Int, in list: List(Int)) -> List(Int) {
    todo
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now provides autocomplete suggestions for labels after
  part of the label has already been typed. For example, in this code:

  ```gleam
  pub type Person {
    Person(name: String, number: Int)
  }

  pub fn main() {
    Person(n|)
  }
  ```

  The language server will provide `name:` and `number:` as autocomplete
  suggestions.

  ([Surya Rose](https://github.com/GearsDatapacks))

### Formatter

### Bug fixes

- Fixed a bug where `case` expressions in custom panic messages would compile
  to invalid syntax on the JavaScript target.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where `case` expressions in custom panic messages would compile
  to invalid syntax on the JavaScript target.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where an underscore after a zero in a number would compile to
  invalid syntax on the JavaScript target.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the "generate function" code action could generate invalid
  code when the same variable was passed as an argument twice.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where replacing a Hex dependency with a Git dependency of the
  same name would cause the build tool to fail.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where updating the remote URL of a Git dependency would fail to
  update the remote in the local dependency, causing a caching issue.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fix slightly wrong error message for missing main function in test module.
  ([Samuel Cristobal](https://github.com/scristobal))

- Fixed a bug where the compiler would not properly warn for unreachable
  patterns in a `case` expression when the clause matched on multiple
  alternative patterns.
  ([Surya Rose](https://github.com/GearsDatapacks))
