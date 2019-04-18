# Gleam ✨

[![build](https://circleci.com/gh/lpil/gleam.svg?style=shield)](https://circleci.com/gh/lpil/gleam)

Gleam is a statically typed functional programming language for building
scalable concurrent systems. It compiles to [Erlang](http://www.erlang.org/)
and has straightforward interop with other BEAM languages such as Erlang,
Elixir and LFE.

```rust
pub enum Tree =
  | Leaf(Int)
  | Node(Tree, Tree)

pub fn any(tree, predicate) {
  case tree {
  | Leaf(i) -> predicate(i)
  | Node(left, right) -> any(left, predicate) || any(right, predicate)
  }
}

pub fn has_even_leaf(tree) {
  any(tree, fn(i) {
    i % 2 == 0
  })
}
```

For more information see the Gleam website: [https://gleam.run](https://gleam.run).
