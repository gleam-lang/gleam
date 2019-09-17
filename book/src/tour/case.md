# Case

The `case` expression is the most common kind of flow control in Gleam code. It
allows us to say "if the data has this shape then do that", which we call
_pattern matching_.

Here we match on an `Int` and return a specific string for the values 0, 1,
and 2. The final pattern `n` matches any other value that did not match any of
the previous patterns.

```rust,noplaypen
case some_number {
| 0 -> "Zero"
| 1 -> "One"
| 2 -> "Two"
| n -> "Some other number" // This matches anything
}
```

Pattern matching on a `Bool` value is the Gleam alternative to the `if else`
statement found in other languages.

```rust,noplaypen
case some_bool {
| True -> "It's true!"
| False -> "It's not true."
}
```

Gleam's `case` is an expression, meaning it returns a value and can be used
anywhere we would use a value. For example, we can name the value of a case
expression with a `let` binding.

```rust,noplaypen
let description =
  case True {
  | True -> "It's true!"
  | False -> "It's not true."
  }

description  // => "It's true!"
```


## Destructuring

A `case` expression can be used to destructure values that
contain other values, such as tuples and lists.

```rust,noplaypen
case xs {
| [] -> "This list is empty"
| [a] -> "This list has 1 element"
| [a, b] -> "This list has 2 element"
| other -> "This list has more than 2 elements"
}
```

It's not just the top level data structure that can be pattern matches,
contained values can also be matched. This gives `case` the ability to
concisely express flow control that might be verbose without pattern matching.

```rust,noplaypen
case xs {
| [[]] -> "The only element is an empty list"
| [[] | _] -> "The 1st element is an empty list"
| [[4] | _] -> "The 1st element is a list of the number 4"
| other -> "Something else"
}
```

Pattern matching also works in `let` bindings, though patterns that do not
match all instances of that type may result in a runtime error.

```rust,noplaypen
let [a] = [1]    // a is 1
let [b] = [1, 2] // Runtime error! The pattern has 1 element but the value has 2
```
