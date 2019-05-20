# Comments

Gleam allows you to write comments in your code.

Here’s a simple comment:

```rust,noplaypen
// Hello, world!
```

In Gleam, comments must start with two slashes and continue until the end of the
line. For comments that extend beyond a single line, you’ll need to include
`//` on each line, like this:

```rust,noplaypen
// Hello, world! I have a lot to say, so much that it will take multiple
// lines of text. Therefore, I will start each line with // to denote it
// as part of a multi-line comment.
```

Comments can also be placed at the end of lines containing code:

```rust,noplaypen
pub fn add(x, y) {
  x + y // here we are adding two values together
}
```

Comments may also be indented:

```rust,noplaypen

pub fn multiply(x, y) {
  // here were are multiplying x by y
  x * y 
}
```
