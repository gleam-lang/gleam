# Bool

A Bool can be either `True` or `False`.

Gleam defines a handful of operators that work with Bools.

```rust,noplaypen
False && False // => False
False && True  // => False
True && False  // => False
True && True   // => True

False || False // => False
False || True  // => True
True || False  // => True
True || True   // => True
```

`&&` and `||` are _short circuiting_, meaning they don't evaluate the right
hand side if they don't have to.

`&&` evaluates the right hand side if the left hand side is `True`.

`||` evaluates the right hand side if the left hand side is `False`.


## Runtime representation

While written in the code using a capital letter, they are represented at
runtime with the atoms `true` and `false`, making them compatible with Elixir
and Erlang's booleans.

This is important if you want to use Gleam and Elixir or Erlang together in
one project.

```rust,noplaypen
// Gleam
True
False
```
```erlang
% Erlang
true.
false.
```
