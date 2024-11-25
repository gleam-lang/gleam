# Changelog

## Unreleased

### Compiler

- Generated Erlang `.app` files now include external modules written in Elixir
  and Erlang.
  ([LostKobrakai](https://github.com/lostkobrakai))

- HexDocs documentation of Gleam packages now uses the ExDocs search data model,
  allowing for global indexing of Gleam packages in HexDocs, and
  making Gleam packages discoverable through global search of HexDocs.
  ([Diemo Gebhardt](https://github.com/diemogebhardt))

- Allow users to set the `GLEAM_CACERTS_PATH` environment variable to specify a
  path to a directory containing CA certificates to install Hex packages.
  ([winstxnhdw](https://github.com/winstxnhdw))

- On the JavaScript target, bit array expressions and patterns no longer need to
  be byte aligned, and the `bits` segment type is now supported in patterns.
  ([Richard Viney](https://github.com/richard-viney))

### Language server

- The language server now has the ability to jump to the type definition of any
  hovered value.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now offers a code action to convert the first step of a
  pipeline to a regular function call. For example, this code:

  ```gleam
  import gleam/list

  pub fn main() {
    [1, 2, 3] |> list.map(fn(n) { n * 2 })
  }
  ```

  Will be rewritten as:

  ```gleam
  import gleam/list

  pub fn main() {
    list.map([1, 2, 3], fn(n) { n * 2 })
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now offers a code action to convert a function call into
  a pipeline. For example, this code:

  ```gleam
  import gleam/list

  pub fn main() {
    list.map([1, 2, 3], fn(n) { n * 2 })
  }
  ```

  Will be rewritten as:

  ```gleam
  import gleam/list

  pub fn main() {
    [1, 2, 3] |> list.map(fn(n) { n * 2 })
  }
  ```

  You can also pick which argument is going to be piped. In this case:

  ```gleam
  import gleam/list

  pub fn main() {
    list.map([1, 2, 3], fn(n) { n * 2 })
    //                   ^ If you put your cursor over here
  }
  ```

  The code will be rewritten as:

  ```gleam
  import gleam/list

  pub fn main() {
    fn(n) { n * 2 } |> list.map([1, 2, 3], _)
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The Language Server now suggests a code action to generate a function to
  encode a custom type as JSON using the `gleam_json` package. For example:

  ```gleam
  pub type Person {
    Person(name: String, age: Int)
  }
  ```

  Will become:

  ```gleam
  import gleam/json

  pub type Person {
    Person(name: String, age: Int)
  }

  fn encode_person(person: Person) -> json.Json {
    json.object([
      #("name", json.string(person.name)),
      #("age", json.int(person.age)),
    ])
  }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- The Language Server now suggests a code action to inline a variable
  which is only used once. For example, this code:

  ```gleam
  import gleam/io

  pub fn main() {
    let greeting = "Hello!"
    io.println(greeting)
  }
  ```

  Will be rewritten as:

  ```gleam
  import gleam/io

  pub fn main() {
    io.println("Hello!")
  }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

### Formatter

### Bug fixes

- Fixed a bug where division and remainder operators would not work correctly
  in guards on the JavaScript target.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the "Generate function" code action would ignore the
  provided labels.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.8.1 - 2025-02-11

### Bug fixes

- Fixed a metadata caching bug where accessors for opaque types could sometimes
  be used in other modules.
  ([Louis Pilfold](https://github.com/lpil))
