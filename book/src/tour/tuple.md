# Tuples

Tuples are an ordered collection of elements of a fixed size. Each element can
be of a different type.

```rust,noplaypen
{"Cat", True}  // Type {String, Bool}
{1, 2.0, "3"}  // Type {Int, Float, String}
```

Contained values can be extracted from structs using a let binding.

```rust,noplaypen
let values = {1, 2.0}
let {x, y} = values

x  // => 1
y  // => 2.0
```
