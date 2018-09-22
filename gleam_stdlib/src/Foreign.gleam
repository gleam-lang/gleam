module Foreign

doc """
Foreign data is data that we don't know the type of yet.
We likely get data like this from interop with Erlang, or from
IO with the outside world.
"""
pub external type Foreign
;

doc """
Convert any Gleam data into Foreign data.
"""
pub external fn new(a) { Foreign } = :"Gleam.Foreign" :identity

doc """
Unsafely cast any type into any other type.o

This is an escape hatch for the type system that may be useful when wrapping
native Erlang APIs. It is to be used as a last measure only.
"""
pub external unsafeCoerce : fn(a) { b } = :"Gleam.Foreign" :identity

fn identity(x) {
  x
}
