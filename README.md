<h1 align="center">
  ✨ Gleam ✨
</h1>

<p align="center">
  <a href="https://circleci.com/gh/lpil/gleam"><img src="https://circleci.com/gh/lpil/gleam.svg?style=shield" alt="build"></a>
  <a href="https://github.com/lpil/gleam/releases"><img src="https://img.shields.io/github/release/lpil/gleam" alt="GitHub release"></a>
  <a href="https://webchat.freenode.net/#gleam-lang"><img src="https://img.shields.io/badge/freenode%20chat-%23gleam--lang-blue" alt="IRC: #gleam-lang on chat.freenode.net"></a>
</p>

<p align="center">
  <b>Fast, friendly, functional!</b>
</p>

<!-- A spacer -->
<div>&nbsp;</div>

Gleam is a statically typed functional programming language for building
scalable concurrent systems. It compiles to [Erlang](http://www.erlang.org/)
and has straightforward interop with other BEAM languages such as Erlang,
Elixir and LFE.

It looks like this:

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
