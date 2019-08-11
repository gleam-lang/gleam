# Structs

Structs are an ordered collection of elements of a fixed size. Each element can
be of a different type.

```rust,noplaypen
struct("Cat", True)  // Type struct(String, Bool)
struct(1, 2.0, "3")  // Type struct(Int, Float, String)
```

Contained values can be extracted from structs using a let binding.

```rust,noplaypen
let values = struct(1, 2.0)
let struct(x, y) = values

x  // => 1
y  // => 2.0
```
