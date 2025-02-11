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

### Language server

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

### Formatter

### Bug fixes
