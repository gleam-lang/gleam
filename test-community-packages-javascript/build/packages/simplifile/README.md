# simplifile

[![Package Version](https://img.shields.io/hexpm/v/simplifile)](https://hex.pm/packages/simplifile)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/simplifile/)

Simplifile provides basic file operations (read, write, append, and delete) that work
for all targets (Erlang, Node, and Deno). It also provides functions for working with directories.

## Example
```gleam
let filepath = "./test/hello.txt"
let assert Ok(_) = "Hello, World" |> write(to: filepath)
let assert Ok(_) = "Goodbye, Mars" |> append(to: filepath)
let assert Ok("Hello, WorldGoodbye, Mars") = read(from: filepath)
let assert Ok(_) = delete(filepath)
let assert Error(_) = read(from: filepath)
```

## Quick start

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```

## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add simplifile
```

and its documentation can be found at <https://hexdocs.pm/simplifile>.
