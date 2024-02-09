# trie_again

[![Package Version](https://img.shields.io/hexpm/v/trie_again)](https://hex.pm/packages/trie_again)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/trie_again/)
![CI](https://github.com/giacomocavalieri/trie_again/workflows/CI/badge.svg?branch=main)

Tries in Gleam 🌳

> ⚙️ This package supports the Erlang and Javascript targets!

## Why tries?

A [trie](https://en.wikipedia.org/wiki/Trie) is a data structure that uses lists as keys. By taking advantage of this property it is possible to efficiently perform some queries: for example imagine you want to find all the elements that are associated with a key with the same prefix; with a trie the complexity of the lookup is linear in the size of the prefix you're looking for.

That is why tries can be used to implement autocompleting text dictionaries, spell checking or prefix matching algorithms.  

In this example a trie is used to store words (as lists of graphemes) as keys associated with a definition. With the `subtrie` function one can look for all the elements sharing a commong prefix in their key:

```gleam
import trie
import string

let dictionary =
  trie.new()
  |> trie.insert(at: string.to_graphemes("gleam"), value: "To produce a small, bright light")
  |> trie.insert(at: string.to_graphemes("gleaming"), value: "Bright and shiny")
  |> trie.insert(at: string.to_graphemes("beam"), value: "A line of light that shines from a bright object")

dictionary
|> trie.subtrie(at: ["g", "l"])
|> trie.to_list
// -> [
//      #(["g", "l", "e", "a", "m"], "To produce a small, bright light"),
//      #(["g", "l", "e", "a", "m", "i", "n", "g"], "Bright and shiny"),
//    ]
```

## Installation

To add this package to your Gleam project:

```sh
gleam add trie_again
```

## Usage

Import the `trie` module and write some code! You can find many examples of how the different functions work in the [project documentation](https://hexdocs.pm/trie_again/).

```gleam
import trie

trie.new()
|> trie.insert(at: ["c", "a", "r"], value: 1)
|> trie.insert(at: ["c", "a", "t"], value: 10)
|> trie.get(at: ["c", "a", "t"])
// -> Ok(10)
```

## Contributing

If you think there's any way to improve this package, or if you spot a bug don't be afraid to open PRs, issues or requests of any kind! Any contribution is welcome 💜
