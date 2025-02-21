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

- Improved the styling of constructor argument descriptions in the generated
  documentation. ([Nicd](https://git.ahlcode.fi/nicd))

- Allow users to set the `GLEAM_CACERTS_PATH` environment variable to specify a
  path to a directory containing CA certificates to install Hex packages.
  ([winstxnhdw](https://github.com/winstxnhdw))

- On the JavaScript target, bit array expressions and patterns no longer need to
  be byte aligned, and the `bits` segment type is now supported in patterns.
  ([Richard Viney](https://github.com/richard-viney))

- The code generated for list pattern matching on the JavaScript target is now
  more efficient. Gleam code that relies heavily on list pattern matching can
  now be up to twice as fast.
  ([yoshi~](https://github.com/yoshi-monster))

- On the JavaScript target, bit array patterns can now match segments of dynamic
  size.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Build tool

- The build tool now supports Git dependencies. For example:

  ```
  [dependencies]
  gleam_stdlib = { git = "https://github.com/gleam-lang/stdlib.git", ref = "957b83b" }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

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

- The code action to generate a dynamic decoder for a custom type can now
  generate decoders for types with multiple variants. For example this code:

  ```gleam
  pub type Person {
    Adult(age: Int, job: String)
    Child(age: Int, height: Float)
  }
  ```

  Becomes:

  ```gleam
  import gleam/dynamic/decode

  pub type Person {
    Adult(age: Int, job: String)
    Child(age: Int, height: Float)
  }

  fn person_decoder() -> decode.Decoder(Person) {
    use variant <- decode.field("type", decode.string)
    case variant {
      "adult" -> {
        use age <- decode.field("age", decode.int)
        use job <- decode.field("job", decode.string)
        decode.success(Adult(age:, job:))
      }
      "child" -> {
        use age <- decode.field("age", decode.int)
        use height <- decode.field("height", decode.float)
        decode.success(Child(age:, height:))
      }
      _ -> decode.failure(todo as "Zero value for Person", "Person")
    }
  }
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

- The language server now suggests a code action to easily interpolate a value
  into a string. If the cursor is inside a literal string the language server
  will offer to split it:

  ```gleam
  "wibble | wobble"
  //      ^ Triggering the action with the cursor
  //        here will produce this:
  "wibble " <> todo <> " wobble"
  ```

  And if the cursor is selecting a valid gleam name, the language server will
  offer to interpolate it as a variable:

  ```gleam
  "wibble wobble woo"
  //      ^^^^^^ Triggering the code action if you're
  //             selecting an entire name, will produce this:
  "wibble " <> wobble <> " woo"
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The Language Server now shows module documentation when hovering over a module
  name.
  ([Surya Rose](https://github.com/GearsDatapacks))

### Formatter

- Redundant function captures that take no additional arguments are now
  rewritten to not use the function capture syntax.

  ```gleam
  some_module.some_function(_)
  ```

  This code is reformatted like so:

  ```gleam
  some_module.some_function
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug fixes

- Fixed a bug where division and remainder operators would not work correctly
  in guards on the JavaScript target.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the "Generate function" code action would ignore the
  provided labels.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the "Pattern match on argument" and
  "Pattern match on variable" code actions would not allow to pattern match on a
  private type used in the same module it's defined in.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where `gleam export package-interface` would not properly generate
  the package interface file if some modules were cached.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where pattern matching using a UTF-8 string constant would not
  work correctly on the JavaScript target when the string contained escape
  characters.
  ([Richard Viney](https://github.com/richard-viney))

## v1.8.1 - 2025-02-11

### Bug fixes

- Fixed a metadata caching bug where accessors for opaque types could sometimes
  be used in other modules.
  ([Louis Pilfold](https://github.com/lpil))
