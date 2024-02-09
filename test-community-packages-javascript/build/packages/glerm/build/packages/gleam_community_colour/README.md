# gleam-community/colour

A package for a standard Colour type, conversions, and other utilities.

[![Package Version](https://img.shields.io/hexpm/v/gleam_community_colour)](https://hex.pm/packages/gleam_community_colour)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleam_community_colour/)

✨ This project is written in pure Gleam so you can use it anywhere Gleam runs: Erlang, Elixir, Node, Deno, and the browser!

---

## Quickstart

```gleam
import gleam_community/colour
import gleam_community/colour/accessibility

pub fn main() {
  let foreground = colour.from_hsl(h: 0.858, s: 1.0, l: 0.843)

  let background_options = [colour.light_grey, colour.dark_grey]

  let background = accessibility.maximum_contrast(foreground, background_options)
}
```

## Installation

`gleam_community` packages are published to [hex.pm](https://hex.pm/packages/gleam_community_colour)
with the prefix `gleam_community_`. You can add them to your Gleam projects directly:

```sh
gleam add gleam_community_colour
```

The docs can be found over at [hexdocs.pm](https://hexdocs.pm/gleam_community_colour).
