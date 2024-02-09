-module(gleam@bit_builder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([new/0, prepend/2, append/2, prepend_builder/2, append_builder/2, prepend_string/2, append_string/2, concat/1, concat_bit_strings/1, from_string/1, from_string_builder/1, from_bit_string/1, to_bit_string/1, byte_size/1]).

-spec new() -> gleam@bytes_builder:bytes_builder().
new() ->
    gleam@bytes_builder:new().

-spec prepend(gleam@bytes_builder:bytes_builder(), bitstring()) -> gleam@bytes_builder:bytes_builder().
prepend(To, Prefix) ->
    gleam@bytes_builder:prepend(To, Prefix).

-spec append(gleam@bytes_builder:bytes_builder(), bitstring()) -> gleam@bytes_builder:bytes_builder().
append(To, Suffix) ->
    gleam@bytes_builder:append(To, Suffix).

-spec prepend_builder(
    gleam@bytes_builder:bytes_builder(),
    gleam@bytes_builder:bytes_builder()
) -> gleam@bytes_builder:bytes_builder().
prepend_builder(To, Prefix) ->
    gleam@bytes_builder:prepend_builder(To, Prefix).

-spec append_builder(
    gleam@bytes_builder:bytes_builder(),
    gleam@bytes_builder:bytes_builder()
) -> gleam@bytes_builder:bytes_builder().
append_builder(First, Second) ->
    gleam_stdlib:iodata_append(First, Second).

-spec prepend_string(gleam@bytes_builder:bytes_builder(), binary()) -> gleam@bytes_builder:bytes_builder().
prepend_string(To, Prefix) ->
    gleam@bytes_builder:prepend_string(To, Prefix).

-spec append_string(gleam@bytes_builder:bytes_builder(), binary()) -> gleam@bytes_builder:bytes_builder().
append_string(To, Suffix) ->
    gleam@bytes_builder:append_string(To, Suffix).

-spec concat(list(gleam@bytes_builder:bytes_builder())) -> gleam@bytes_builder:bytes_builder().
concat(Builders) ->
    gleam_stdlib:identity(Builders).

-spec concat_bit_strings(list(bitstring())) -> gleam@bytes_builder:bytes_builder().
concat_bit_strings(Bits) ->
    gleam_stdlib:identity(Bits).

-spec from_string(binary()) -> gleam@bytes_builder:bytes_builder().
from_string(String) ->
    gleam_stdlib:wrap_list(String).

-spec from_string_builder(gleam@string_builder:string_builder()) -> gleam@bytes_builder:bytes_builder().
from_string_builder(Builder) ->
    gleam_stdlib:wrap_list(Builder).

-spec from_bit_string(bitstring()) -> gleam@bytes_builder:bytes_builder().
from_bit_string(Bits) ->
    gleam_stdlib:wrap_list(Bits).

-spec to_bit_string(gleam@bytes_builder:bytes_builder()) -> bitstring().
to_bit_string(Builder) ->
    erlang:list_to_bitstring(Builder).

-spec byte_size(gleam@bytes_builder:bytes_builder()) -> integer().
byte_size(Builder) ->
    erlang:iolist_size(Builder).
