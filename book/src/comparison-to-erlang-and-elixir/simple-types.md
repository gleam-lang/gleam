# Simple types

Gleam has many of the same data types as Erlang and Elixir, with a few
differences.


## Strings

Gleam's Strings are binary strings, the same as Elixir's strings.

```
// Gleam
"Hello, Gleam!"
```
```
# Elixir
"Hello, Elixir!"
```
```
% Erlang
<<"Hello, Erlang!">>.
```


## Ints

Gleam's Ints are the same as Elixir and Erlang's integers.

```
// Gleam
123
```
```
# Elixir
123
```
```
% Erlang
123.
```


## Floats

Gleam's Floats are the same as Elixir and Erlang's floats.

```
// Gleam
1.5
```
```
# Elixir
1.5
```
```
% Erlang
1.5.
```


## Bools

Gleam's Bools are the same as Elixir and Erlang's booleans.

While written in the code using a capital letter, they are represented at
runtime with the atoms `true` and `false`, making them compatible with Elixir
and Erlang's booleans.

```
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


## Atoms

Gleam does not currently have first class support for Erlang atoms, though
likely will in future.


## Tuples

Tuples are the same in Gleam as in Elixir and Erlang.

```
// Gleam
{1, 2.0, "three"}
```
```
# Elixir
true
{1, 2.0, "three"}
```
```
% Erlang
{1, 2.0, <<"three">>}.
```


## Lists

Unlike in Elixir and Erlang, Gleam's Lists are _homogeneous_, meaning all the
elements of a List must be of the same type.

Attempting to construct a list of multiple types of element will result in the
compiler presenting a type error.

```
// Gleam
[1, 2, 3, 4]  // List(Int)
[1.22, 2.30]  // List(Float)
[1.22, 3, 4]  // Type error!
```
```
# Elixir
[1, 2, 3, 4]
[1.22, 2.30]
[1.22, 3, 4]
```
```
% Erlang
[1, 2, 3, 4].
[1.22, 2.30].
[1.22, 3, 4].
```

The syntax for prepending to a List is the same as in Elixir or Erlang.

```
// Gleam
[1 | [2, 3]]  // => [1, 2, 3]
```
```
# Elixir
[1 | [2, 3]]
```
```
% Erlang
[1 | [2, 3]].
```
