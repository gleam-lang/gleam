-module(gleam@bit_string).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([from_string/1, byte_size/1, append/2, slice/3, is_utf8/1, to_string/1, concat/1]).

-spec from_string(binary()) -> bitstring().
from_string(X) ->
    gleam_stdlib:identity(X).

-spec byte_size(bitstring()) -> integer().
byte_size(X) ->
    erlang:byte_size(X).

-spec append(bitstring(), bitstring()) -> bitstring().
append(First, Second) ->
    gleam@bit_array:append(First, Second).

-spec slice(bitstring(), integer(), integer()) -> {ok, bitstring()} |
    {error, nil}.
slice(String, Position, Length) ->
    gleam_stdlib:bit_array_slice(String, Position, Length).

-spec is_utf8(bitstring()) -> boolean().
is_utf8(Bits) ->
    gleam@bit_array:is_utf8(Bits).

-spec to_string(bitstring()) -> {ok, binary()} | {error, nil}.
to_string(Bits) ->
    gleam@bit_array:to_string(Bits).

-spec concat(list(bitstring())) -> bitstring().
concat(Bit_strings) ->
    gleam_stdlib:bit_array_concat(Bit_strings).
