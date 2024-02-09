# non_empty_list

[![Package Version](https://img.shields.io/hexpm/v/non_empty_list)](https://hex.pm/packages/non_empty_list)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/non_empty_list/)
![CI](https://github.com/giacomocavalieri/non_empty_list/workflows/CI/badge.svg?branch=main)

Non-empty lists in Gleam ✨

> ⚙️ This package supports both the Erlang and JavaScript target!

## Installation

To add this package to your Gleam project:

```sh
gleam add non_empty_list
```

## Usage

Import the `non_empty_list` module and write some code!

```gleam
import non_empty_list
import gleam/int
import gleam/io

pub fn main() {
  non_empty_list.new(1, [2, 3, 4])
  |> non_empty_list.reduce(with: fn(n, m) { n + m })
  |> int.to_string
  |> io.println 
}
```

## Contributing

This package exposes most of the same functions you'd find in the `gleam/list` module but it may be missing some useful functions.

If you think there's a missing function that would fit here, or if you spot a bug don't be afraid to open PRs, issues or requests of any kind! Any contribution is welcome 💜
