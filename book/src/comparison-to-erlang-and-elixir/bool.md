# Bool

Gleam's Bools are the same as Elixir and Erlang's booleans.

While written in the code using a capital letter, they are represented at
runtime with the atoms `true` and `false`, making them compatible with Elixir
and Erlang's booleans.

```rust,noplaypen
// Gleam
True
False
```
```
# Elixir
true
false
```
```
% Erlang
true.
false.
```

Gleam defines a handful of operators that operate on or return Bools.

- `==`, `!=`: Equality
- `<`, `<=`, `>=`, `>`: Order comparison
- `&&`: Short circuiting logical and
- `||`: Short circuiting logical or
