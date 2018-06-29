module Foreign exposing Foreign, new/1

doc """
Foreign data is data that we don't know the type of yet.
We likely get data like this from interop with Erlang, or from
IO with the outside world.
"""
external type Foreign

doc """
Convert any Gleam data into Foreign data.
"""
external new : |a| -> Foreign = :gleam_native.identity

doc """
Unsafely cast any type into any other type.o

This is an escape hatch for the type system that may be useful when wrapping
native Erlang APIs. It is to be used as a last measure only.
"""
external unsafeCoerce : |a| -> b = :gleam_native.identity
