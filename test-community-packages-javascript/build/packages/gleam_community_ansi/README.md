# gleam-community/ansi

Format text with ANSI escape sequences.

[![Package Version](https://img.shields.io/hexpm/v/gleam_community_ansi)](https://hex.pm/packages/gleam_community_ansi)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleam_community_ansi/)

✨ This project is written in _pure Gleam_ so you can use it anywhere Gleam runs:
Erlang, Elixir, Node, Deno, even [some browsers](https://bit.ly/devtools-console-ansi-support)!

---

## Quickstart

```gleam
import gleam/io
import gleam_community/ansi

pub fn main() {
  let greeting = "Hello, " <> ansi.pink("world") <> "!"

  greeting
  |> ansi.bg_white
  |> io.println
}

```

## Installation

`gleam_community` packages are published to [hex.pm](https://hex.pm/packages/gleam_community_ansi)
with the prefix `gleam_community_`. You can add them to your Gleam projects directly:

```sh
gleam add gleam_community_ansi
```

The docs can be found over at [hexdocs.pm](https://hexdocs.pm/gleam_community_ansi).

## ANSI-what?

ANSI escape sequences date back to the 70s as a standard way to format text on
various video text terminals. Since then they have been adopted by many software
terminal emulators and platforms, including some Web browsers, and are a simple
way to format text without platform-specific APIs.

The point of this package is to abstract the specific codes away and give you an
easy-to-understand API for formatting and colouring terminal text. Still, here is
a quick couple of examples of what's happening under the hood.

You can copy these examples straight into your terminal to see them in action!

- Colour text yellow:

  ```shell
  $ echo "\e[33mhello"
  ```

- Colour the background pink:

  ```shell
  $ echo "\e[45mhello"
  ```

- Render text italic:

  ```shell
  $ echo "\e[3mhello\e[23m"
  ```

As you can see, the escape sequences are a bit obscure. Sure, you could hard code
them, or you could use this package!
