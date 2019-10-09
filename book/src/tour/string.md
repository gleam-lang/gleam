# String

Gleam's has UTF-8 binary strings, written as text surrounded by double quotes.

```rust,noplaypen
"Hello, Gleam!"
```

Strings can span multiple lines.

```rust,noplaypen
"Hello
Gleam!"
```

Special characters such as `"` need to be escaped with a `\` character.

```rust,noplaypen
"Here is a double quote -> \" <-"
```

## Standard library

To use standard library for 'string', add:
```
import gleam/string
```

This package includes simple functions to manipulate 'string' variable.

### Methods

- `fn reverse(string)`: 
    Reverse the order of characters in string

- `fn replace(string, pattern, with)`:
    Returns a new string with some or all matches of a pattern replaced by a replacement

// TODO
