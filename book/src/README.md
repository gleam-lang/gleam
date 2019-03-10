# Gleam

Gleam is a statically typed functional programming language for building
scalable concurrent systems.

It compiles to [Erlang](http://www.erlang.org/) and has straightforward
interop with other BEAM languages such as Erlang, Elixir and LFE.

It looks like this:


```rust,noplaypen
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

## Principles

### Be safe

An expressive type system inspired by the ML family of languages helps us find
and prevent bugs at compile time, long before it reaches your users.

For the problems the type system can't solve (such as your server being hit by
a bolt of lightning) the Erlang/OTP runtime provides well tested mechanisms
for gracefully handling failure.


### Be friendly

Hunting down bugs can be stressful so feedback from the compiler should be
as clear and helpful as possible. We want to spend more time working on our
application and less time looking for typos or deciphering cryptic error
messages.

As a community we want to be friendly too. People of all backgrounds, genders,
and experience levels are welcome and must receive equal respect.


### Be performant

The Erlang/OTP runtime is known for its speed and ability to scale, enabling
organisations such as WhatsApp and Ericsson to reliably handle massive amounts
of traffic at low latency. Gleam should take full advantage of this runtime
and be as fast as other BEAM languages such as Erlang and Elixir.


### Be a good citizen

Gleam makes it easy to use code written in other BEAM languages such as
Erlang, Elixir and LFE, so there's a rich ecosystem of tools and library for
Gleam users to make use of.

Users of other BEAM languages should in return be able to take advantage of
Gleam, either by transparently making use of libraries written in Gleam, or by
adding Gleam modules to their existing project with minimal fuss.
