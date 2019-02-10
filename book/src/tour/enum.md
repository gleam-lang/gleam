# Enums

Enums in Gleam are a way of modeling data that can be one of a few different
variants. They must be declared before use, and the names of variants must be
unique for the given module.

We've seen an enum already in this chapter- `Bool`.

Bool is defined like this:

```rust,noplaypen
// A Bool is a value that is either `True` or `False`
enum Bool =
  | True
  | False
```


Enum variants can also contain other values, and these values can be extracted
using a let binding.

```rust,noplaypen
enum User =
  | LoggedIn(String)  // A logged in user with a name
  | Guest             // A guest user with no details
```
```rust,noplaypen
let sara = LoggedIn("Sara")
let rick = LoggedIn("Rick")
let visitor = Guest
```


## Destructuring

When given an enum we can pattern match on it to determine which variant it is
and to assign names to any contained values.

```rust,noplaypen
fn get_name(user) {
  case user {
  | LoggedIn(name) -> name
  | Guest -> "Guest user"
  }
}
```

Enums can also be destructured with a `let` binding.

```rust,noplaypen
enum Score =
  | Points(Int)
```
```rust,noplaypen
let score = Points(50)
let Points(p) = score

p // => 50
```


## Erlang interop

At runtime enum variants with no contained values become atoms. The atoms are
written in `snake_case` rather than `CamelCase` so `LoggedIn` becomes
`logged_in`.

Enum variants with contained values become tuples with a tag atom.

```rust,noplaypen
// Gleam
Guest
LoggedIn("Kim")
```
```
# Elixir
:guest
{:logged_in, "Kim"}
```
```
% Erlang
guest.
{logged_in, <<"Kim">>}.
```
