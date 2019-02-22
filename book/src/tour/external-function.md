# External function

Gleam is just one of many languages on the Erlang virtual machine and at times
we may want to use functions from these other languages in our Gleam programs.
To enable this Gleam allows the importing of _external functions_, which may
be written in any BEAM language.

External functions are typically written in a different language with a
different type system, so the compiler is unable to determine the type of the
function and instead the programmer must inform the compiler the type.

Gleam trusts that the type given is correct so an inaccurate type annotation
can result in unexpected behaviour and crashes at runtime. Be careful!


## Example

The Erlang `rand` module has a function named `uniform` that takes no
arguments and returns a `Float`.

The Elixir module `IO` has a function named `inspect` that takes any value,
prints it, and returns the same value.

If we want to import these functions and use them in our program we would do
so like this:

```rust,noplaypen
pub external fn random_float() -> Float = "rand" "uniform"

// Elixir modules start with `Elixir.`
pub external fn inspect(a) -> a = "Elixir.IO" "inspect"
```
