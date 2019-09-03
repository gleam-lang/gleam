# Gleam ✨

[![build](https://circleci.com/gh/lpil/gleam.svg?style=shield)](https://circleci.com/gh/lpil/gleam)
[![GitHub release](https://img.shields.io/github/release/lpil/gleam)](https://github.com/lpil/gleam/releases)
[![IRC: #gleam-lang on chat.freenode.net](https://img.shields.io/badge/freenode%20chat-%23gleam--lang-blue)](https://webchat.freenode.net/#gleam-lang)


Gleam is a statically typed functional programming language for building
scalable concurrent systems. It compiles to [Erlang](http://www.erlang.org/)
and has straightforward interop with other BEAM languages such as Erlang,
Elixir and LFE.

```rust
pub enum Tree(value) =
  | Leaf(value)
  | Node(Tree(value), Tree(value))

pub fn any(tree: Tree(a), check: fn(a) -> Bool) -> Bool {
  case tree {
  | Leaf(i) -> check(i)
  | Node(left, right) -> any(left, check) || any(right, check)
  }
}

pub fn has_even_leaf(tree: Tree(Int)) -> Bool {
  any(tree, fn(i) {
    i % 2 == 0
  })
}
```

For more information see the Gleam website: [https://gleam.run](https://gleam.run).
