# Changelog

## Unreleased

### Compiler

- You can now use the `echo` keyword to debug print any value: `echo` can be
  followed by any expression and it will print it to stderr alongside the module
  it comes from and its line number. This:

  ```gleam
  pub fn main() {
    echo [1, 2, 3]
  }
  ```

  Will output to stderr:

  ```txt
  /src/module.gleam:2
  [1, 2, 3]
  ```

  `echo` can also be used in the middle of a pipeline. This:

  ```gleam
  pub fn main() {
    [1, 2, 3]
    |> echo
    |> list.map(fn(x) { x * 2 })
    |> echo
  }
  ```

  Will output to stderr:

  ```txt
  /src/module.gleam:3
  [1, 2, 3]
  /src/module.gleam:5
  [2, 4, 6]
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

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

- The build tool now refuses to publish any incomplete package that has any
  `echo` debug printing left.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

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

- The Language Server now provides the ability to rename local variables.
  For example:

  ```gleam
  pub fn main() {
    let wibble = 10
    //  ^ If you put your cursor here, and trigger a rename
    wibble + 1
  }
  ```

  Triggering a rename and entering `my_number` results in this code:

  ```gleam
  pub fn main() {
    let my_number = 10
    my_number + 1
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
