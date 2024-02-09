# gloml

[![Package Version](https://img.shields.io/hexpm/v/gloml)](https://hex.pm/packages/gloml)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gloml/)

A toml parsing library for gleam ✨ All gleam targets and runtimes are supported by gloml.

*Timestamps are not currently supported.*

```gleam
import gleam/io

pub fn main() {
  decode(
    "
[my-project]
version = \"1.2.3\"
",
    d.field("my-project", d.field("version", d.string)),
  )
  |> io.println()
}
```

## Quick start

```sh
cd priv; npm install; cd .. # install dependencies for js
gleam test  # Run the tests
```

## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add gloml
```

and its documentation can be found at <https://hexdocs.pm/gloml>.
