# argv

A cross platform library for getting the command line arguments.

[![Package Version](https://img.shields.io/hexpm/v/argv)](https://hex.pm/packages/argv)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/argv/)

```sh
gleam add argv@1
```

```gleam
import argv
import gleam/io

pub fn main() {
  case argv.load().arguments {
    ["hello", name] ->
      io.println("Hello, " <> name <> "!")
    _ ->
      io.println("usage: ./program hello <name>")
  }
}
```
