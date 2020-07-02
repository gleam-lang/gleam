<p align="center">
  <img src="img/gleam-logo-readme.png" alt="Gleam logo">
</p>

<h1 align="center">
  Fast, friendly, functional!
</h1>

<p align="center">
  <a href="https://github.com/gleam-lang/gleam/releases"><img src="https://img.shields.io/github/release/gleam-lang/gleam" alt="GitHub release"></a>
  <a href="https://webchat.freenode.net/#gleam-lang"><img src="https://img.shields.io/badge/freenode%20chat-%23gleam--lang-blue" alt="IRC: #gleam-lang on chat.freenode.net"></a>
  <a><img src="https://github.com/gleam-lang/gleam/workflows/CI/badge.svg?branch=main"></a>
</p>


<!-- A spacer -->
<div>&nbsp;</div>

Gleam is a statically typed functional programming language for building
scalable concurrent systems. It compiles to [Erlang](http://www.erlang.org/)
and has straightforward interop with other BEAM languages such as Erlang,
Elixir and LFE.

It looks like this:

```rust
pub type Tree(value) {
  Leaf(value)
  Branch(Tree(value), Tree(value))
};

pub fn any(tree: Tree(a), check: fn(a) -> Bool) -> Bool {
  case tree {
    Leaf(i) -> check(i)
    Branch(left, right) -> any(left, check) || any(right, check)
  }
}

pub fn has_even_leaf(tree: Tree(Int)) -> Bool {
  any(tree, fn(i) {
    i % 2 == 0
  })
}
```

For more information see the Gleam website: [https://gleam.run](https://gleam.run).

## Sponsors

Gleam is kindly supported by its sponsors. If you would like to support Gleam
please consider sponsoring its development [on GitHub](https://github.com/sponsors/lpil).

These people are sponsoring at least $10 a month, enabling further Gleam
development.

- [Arian Daneshvar](https://github.com/bees)
- [Ben Myles](https://github.com/benmyles)
- [Bryan Paxton](https://github.com/starbelly)
- [Christian Meunier](https://github.com/tlvenn)
- [Clever Bunny LTD](https://github.com/cleverbunny)
- [Dave Lucia](https://github.com/davydog187)
- [David McKay](https://github.com/rawkode)
- [Guilherme Pasqualino](https://github.com/ggpasqualino)
- [Hasan YILDIZ](https://github.com/hsnyildiz)
- [Hendrik Richter](https://github.com/hendi)
- [Hécate](https://github.com/Kleidukos)
- [James MacAulay](https://github.com/jamesmacaulay)
- [John Palgut](https://github.com/Jwsonic)
- [José Valim](https://github.com/josevalim)
- [Lars Wikman](https://github.com/lawik)
- [Mario Vellandi](https://github.com/mvellandi)
- [Mike R](https://github.com/mroach)
- [Parker Selbert](https://github.com/sorentwo)
- [Quinn Wilton](http://quinnwilton.com/)
- [Sean Jensen-Grey](https://github.com/seanjensengrey)
- [Shritesh Bhattarai](https://github.com/shritesh)
- [Wojtek Mach](https://github.com/wojtekmach)
- [mario](https://github.com/mario-mazo)
- [ontofractal](https://github.com/ontofractal)
