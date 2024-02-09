# hug

A package for creating helpful, and pretty CLI messages.

[![Package Version](https://img.shields.io/hexpm/v/hug)](https://hex.pm/packages/hug)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/hug/)

✨ This project is written in pure Gleam so you can use it anywhere Gleam runs: Erlang, Elixir, Node, Deno, and the browser!

---

## Quick start

```gleam
import gleam/io
import hug

pub fn main() {
  let source = "let six = 5 + 1.0"

   source
   |> hug.error(
     in: "example.gleam",
     from: #(1, 10),
     to: #(1, 17),
     message: "invalid type",
     hint: "can not add an `Int` to a `Float`"
   )
   |> io.println()
  )
}
```

## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add hug
```

and its documentation can be found at <https://hexdocs.pm/hug>.


## Why `hug`?

The name `hug` is inspired by Mark Rendle's talk [The Worst Programming Language Ever](https://youtu.be/vcFBwt1nu2U?t=2229) where he refers error messages in Rust as "a hug from the compiler".
