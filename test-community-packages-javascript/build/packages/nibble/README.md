# nibble

[![Package Version](https://img.shields.io/hexpm/v/nibble)](https://hex.pm/packages/nibble)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/nibble/)

A string parsing library heavily inspired by [`elm/parser`](https://github.com/elm/parser).

## Quick start

```gleam
import gleam/function
import nibble.{ Parser }

type Point {
    Point(x: Int, y: Int)
}

pub fn main () {
    let parser = 
        nibble.succeed(function.curry2(Point))
            |> nibble.drop(nibble.grapheme("("))
            |> nibble.drop(nibble.spaces())
            |> nibble.keep(nibble.int())
            |> nibble.drop(nibble.spaces())
            |> nibble.drop(nibble.grapheme(","))
            |> nibble.drop(nibble.spaces())
            |> nibble.keep(nibble.int())
            |> nibble.drop(nibble.spaces())
            |> nibble.drop(nibble.grapheme(")"))

    assert Ok(point) = nibble.run("(1, 2)", parser)

    point.x //=> 1
    point.y //=> 2
}
```

## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add nibble
```

and its documentation can be found at <https://hexdocs.pm/nibble>.
