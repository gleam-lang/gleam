# Variables

A value can be given a name using `let`. Names can be reused by later let
bindings, but the values contained are _immutable_, meaning the values
themselves cannot be changed.

```rust,noplaypen
// Gleam
let x = 1
let x = 2
```
```
# Elixir
x = 1
x = 2
```
```
% Erlang
X = 1.
X = 2.  % Runtime error! Erlang doesn't allow rebinding
```

Pattern matching can be used to extract contained values from data structures
when defining variables with `let`.

```rust,noplaypen
// Gleam
let {x, y} = {1, 2.0}
```
```
# Elixir
{x, y} = {1, 2.0}
```
```
% Erlang
{X, Y} = {1, 2.0}.
```
