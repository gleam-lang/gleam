# Variables

Gleam's variables are immutable and rebindable, like Elixir's.

```
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
X = 2.  % Runtime error!
```

Pattern matching can be used to extract contained values from data structures
when defining variables with `let`.

```
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
