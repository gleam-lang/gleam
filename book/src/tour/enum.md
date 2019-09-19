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

## Commonly used enums

### `Bool`

```rust,noplaypen
pub enum Bool =
  | True
  | False
```

As seen above Gleam's `Bool` type is an enum! Use it to answer yes/no
questions and to indicate whether something is `True` or `False`.


### `Result(value, error)`

```rust,noplaypen
pub enum Result(value, reason) =
  | Ok(value)
  | Error(reason)
```

Gleam doesn't have exceptions of `null` to represent errors in our programs,
instead we have the `Result` type. If a function call fail wrap the returned
value in a `Result`, either `Ok` if the function was successful, or `Error`
if it failed.

```rust,noplaypen
pub fn lookup(name, phone_book) {
  // ... we found a phone number in the phone book for the given name here
  Ok(phone_number)
}
```

The `Error` type needs to be given a reason for the failure in order to
return, like so:

```rust,noplaypen
pub enum MyDatabaseError =
  | InvalidQuery
  | NetworkTimeout

pub fn insert(db_row) {
  // ... something went wrong connecting to a database here
  Error(NetworkTimeout)
}
```

In cases where we don't care about the specific error enough to want to create
a custom error type, or when the cause of the error is obvious without further
detail, the `Nil` type can be used as the `Error` reason.

```rust,noplaypen
pub fn lookup(name, phone_book) {
  // ... That name wasn't found in the phone book
  Error(Nil)
}
```

When we have a `Result` type returned to us from a function we can pattern
match on it using `case` to determine whether we have an `Ok` result or
an `Error` result.

The standard library `gleam/result` module contains helpful functions for
working with the `Result` type, make good use of them!


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
guest,
{logged_in, <<"Kim">>}.
```
