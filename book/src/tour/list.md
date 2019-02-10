# List

Lists are ordered collections of values. They're one of the most common data
structures in Gleam.

Lists are _homogeneous_, meaning all the elements of a List must be of the
same type. Attempting to construct a list of multiple types of element will
result in the compiler presenting a type error.

```rust,noplaypen
// Gleam
[1, 2, 3, 4]  // List(Int)
[1.22, 2.30]  // List(Float)
[1.22, 3, 4]  // Type error!
```

Prepending to a list is very fast, and is the preferred way to add new values.

```rust,noplaypen
// Gleam
[1 | [2, 3]]  // => [1, 2, 3]
```
