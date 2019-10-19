# Struct

Gleam's struct types are named collections of keys and values. They are
similar to objects in object oriented languages, though they don't have
methods.

Structs are defined with the `struct` keyword.

```rust,noplaypen
pub struct Cat {
  name: String
  cuteness: Int
}
```

Here we have defined a struct called `Cat` which has two fields: A `name`
field which is a `String`, and a `cuteness` field which is an `Int`.

The `pub` keyword has been used to make this struct usable from other modules.

Once defined the struct type can be used in functions:

```rust,noplaypen
fn cats() {
  // Struct fields can be given in any order
  let cat1 = Cat(name: "Nubi", cuteness: 2001)
  let cat2 = Cat(cuteness: 1805, name: "Biffy")

  // Alternatively fields can be given without labels
  let cat3 = Cat("Ginny", 1950)

  [cat1, cat2, cat3]
}
```


## Destructuring structs

To extract values from structs we can pattern match on them with `let` or
`case`.

```rust,noplaypen
let Cat(name: name1, cuteness: cuteness1) = cat1

name1     // => "Nubi"
cuteness1 // => 2001
```

We can also pattern match using positional arguments:

```rust,noplaypen
let Cat(name2, cuteness2) = cat2

name2     // => "Biffy"
cuteness2 // => 1805
```


## Generic structs

Structs types can be parameterised so the same struct can be constructed with
different contained types.

```rust,noplaypen
pub struct Box(a) {
  tag: String,
  contents: a, // The type of this field is injected when constructed
}

fn run() {
  Box(tag: "one", contents: 1.0) // the type is Box(Float)
  Box(tag: "two", contents: "2") // the type is Box(String)
}
```


## Commonly used structs

### `Pair(a, b)`

There may be times when you want to move two values together, or return them
both from a function. To save you from having to define a new struct for this
the Gleam standard library implements a `Pair` type in the `gleam/pair`
module which can contain any two values.

```rust,noplaypen
import gleam/pair

fn run() {
  pair.Pair("ok", 100) // type is Pair(String, Int)
  pair.Pair(1.01, [1]) // type is Pair(Float, List(Int))
}
```


## Erlang interop

At runtime Gleam structs are Erlang tuples. All information about the names of
the keys is erased at runtime.
