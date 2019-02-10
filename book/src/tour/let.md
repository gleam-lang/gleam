# Let bindings

A value can be given a name using `let`. Names can be reused by later let
bindings, but the values contained are _immutable_, meaning the values
themselves cannot be changed.

```rust,noplaypen
let x = 1
let y = x
let x = 2

x  // => 2
y  // => 1
```
